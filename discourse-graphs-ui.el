;;; discourse-graphs-ui.el --- Web UI for Discourse Graphs -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025

;; Author: Your Name
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (websocket "1.13"))
;; Keywords: org-mode, knowledge-graph, visualization
;; URL: https://github.com/yourusername/discourse-graphs

;;; Commentary:

;; This package provides a modern web-based visualization interface for
;; discourse graphs. It integrates with discourse-graphs.el via WebSocket
;; to display nodes and relationships in an interactive force-directed graph.

;; Features:
;; - Real-time WebSocket connection
;; - Interactive force-directed graph visualization
;; - Search and filter by node type (Question, Claim, Evidence, Source)
;; - Dynamic node type coloring
;; - Follow mode integration with Emacs
;; - Professional UI with smooth animations
;; - Relation type filtering (supports, opposes, informs, answers)

;;; Code:

(require 'websocket)
(require 'json)
;; Note: This file is loaded by discourse-graphs.el
;; Do NOT require discourse-graphs here to avoid circular dependency

(defgroup discourse-graphs-ui nil
  "Web UI for discourse graphs."
  :group 'discourse-graphs
  :prefix "dg-ui-")

(defcustom dg-ui-port 35904
  "Port for WebSocket server."
  :type 'integer
  :group 'discourse-graphs-ui)

(defcustom dg-ui-host "localhost"
  "Host for WebSocket server."
  :type 'string
  :group 'discourse-graphs-ui)

(defcustom dg-ui-html-file
  (let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
         ;; Try multiple locations to support different package managers
         (paths (list
                 ;; Path 1: Relative to this file (normal installation)
                 (expand-file-name "discourse-graphs-ui/out/index.html" this-dir)
                 ;; Path 2: straight.el repos directory
                 (expand-file-name "straight/repos/discourse-graphs/discourse-graphs-ui/out/index.html" 
                                   user-emacs-directory)
                 ;; Path 3: straight.el build directory
                 (expand-file-name "straight/build/discourse-graphs/discourse-graphs-ui/out/index.html"
                                   user-emacs-directory))))
    (or (seq-find #'file-exists-p paths)
        (car paths))) ; Default to first path if none exist
  "Path to the UI HTML file."
  :type 'file
  :group 'discourse-graphs-ui)

(defvar dg-ui--server nil
  "WebSocket server instance.")

(defvar dg-ui--clients nil
  "List of connected WebSocket clients.")

(defvar dg-ui--auto-update-timer nil
  "Timer for automatic graph updates.")

;;; Server Functions

(defun dg-ui--send-to-clients (message)
  "Send MESSAGE to all connected clients."
  (dolist (client dg-ui--clients)
    (when (websocket-openp client)
      (condition-case err
          (websocket-send-text client (json-encode message))
        (error
         (message "DG-UI: Error sending to client: %s" (error-message-string err)))))))

(defun dg-ui--format-node (node-plist)
  "Format NODE-PLIST for UI consumption."
  (let ((id (plist-get node-plist :id))
        (title (plist-get node-plist :title))
        (type (plist-get node-plist :type))
        (file (plist-get node-plist :file)))
    `((id . ,id)
      (title . ,title)
      (type . ,type)
      (file . ,file))))

(defun dg-ui--get-graph-data ()
  "Get current graph data from discourse-graphs."
  (let* ((all-nodes (dg-all-nodes))
         (all-relations (dg-all-relations))
         (nodes (mapcar #'dg-ui--format-node all-nodes))
         (links (mapcar (lambda (rel)
                         (let* ((rel-type (plist-get rel :type))
                                (rel-info (alist-get (intern rel-type) dg-relation-types))
                                (style (or (plist-get rel-info :style) "solid")))
                           `((source . ,(plist-get rel :from))
                             (target . ,(plist-get rel :to))
                             (type . ,rel-type)
                             (style . ,style))))
                       all-relations)))
    `((nodes . ,(vconcat nodes))
      (links . ,(vconcat links)))))

(defun dg-ui--handle-message (client frame)
  "Handle incoming message from CLIENT in FRAME."
  (let* ((msg (json-read-from-string (websocket-frame-text frame)))
         (msg-type (alist-get 'type msg)))
    (cond
     ((string= msg-type "requestGraphData")
      (let ((data (dg-ui--get-graph-data)))
        (websocket-send-text client
                            (json-encode `((type . "graphdata")
                                         (data . ,data))))))
     
     ((string= msg-type "open")
      (let ((id (alist-get 'id (alist-get 'data msg))))
        (when id
          (dg-ui-open-node id)))))))

(defun dg-ui--on-client-open (client)
  "Handle new CLIENT connection."
  (push client dg-ui--clients)
  (message "DG-UI: Client connected (%d total)" (length dg-ui--clients))
  ;; Send initial data
  (let ((data (dg-ui--get-graph-data)))
    (websocket-send-text client
                        (json-encode `((type . "graphdata")
                                     (data . ,data))))))

(defun dg-ui--on-client-close (client)
  "Handle CLIENT disconnection."
  (setq dg-ui--clients (delete client dg-ui--clients))
  (message "DG-UI: Client disconnected (%d remaining)" (length dg-ui--clients)))

;;;###autoload
(defun dg-ui-start-server ()
  "Start the WebSocket server."
  (interactive)
  (when dg-ui--server
    (dg-ui-stop-server))
  
  (setq dg-ui--server
        (websocket-server
         dg-ui-port
         :host dg-ui-host
         :on-message #'dg-ui--handle-message
         :on-open #'dg-ui--on-client-open
         :on-close #'dg-ui--on-client-close))
  
  (message "DG-UI: Server started on %s:%d" dg-ui-host dg-ui-port))

;;;###autoload
(defun dg-ui-stop-server ()
  "Stop the WebSocket and HTTP servers."
  (interactive)
  (when dg-ui--auto-update-timer
    (cancel-timer dg-ui--auto-update-timer)
    (setq dg-ui--auto-update-timer nil))
  (when dg-ui--server
    (websocket-server-close dg-ui--server)
    (setq dg-ui--server nil
          dg-ui--clients nil)
    (message "DG-UI: WebSocket server stopped"))
  ;; Stop HTTP server
  (dg-ui--stop-http-server))

;;; Graph Update Functions

(defun dg-ui-refresh ()
  "Refresh the graph data in connected clients."
  (interactive)
  (when dg-ui--clients
    (let ((data (dg-ui--get-graph-data)))
      (dg-ui--send-to-clients `((type . "graphdata")
                                (data . ,data))))))

(defun dg-ui-follow-node (id)
  "Send follow command for node ID."
  (dg-ui--send-to-clients `((type . "command")
                            (data . ((commandName . "follow")
                                   (id . ,id))))))

;;; Auto-update

(defun dg-ui-enable-auto-update (&optional interval)
  "Enable automatic graph updates every INTERVAL seconds (default 5)."
  (interactive)
  (when dg-ui--auto-update-timer
    (cancel-timer dg-ui--auto-update-timer))
  (setq dg-ui--auto-update-timer
        (run-with-timer (or interval 5) (or interval 5) #'dg-ui-refresh))
  (message "DG-UI: Auto-update enabled (every %d seconds)" (or interval 5)))

(defun dg-ui-disable-auto-update ()
  "Disable automatic graph updates."
  (interactive)
  (when dg-ui--auto-update-timer
    (cancel-timer dg-ui--auto-update-timer)
    (setq dg-ui--auto-update-timer nil)
    (message "DG-UI: Auto-update disabled")))

;;; Node Opening

(defun dg-ui-open-node (id)
  "Open node with ID in Emacs."
  (let ((node (dg-get id)))
    (if node
        (let ((file (plist-get node :file))
              (pos (plist-get node :pos)))
          (when (and file (file-exists-p file))
            (find-file file)
            ;; Use org-id-goto for accurate positioning
            (condition-case nil
                (org-id-goto id)
              (error
               ;; Fallback: use stored position
               (when pos
                 (goto-char pos))))
            (org-show-context)
            (recenter)
            (message "Opened: %s" (plist-get node :title))))
      (message "Node not found: %s" id))))

;;; UI Integration

(defcustom dg-ui-http-port 8080
  "Port for the HTTP server serving UI files."
  :type 'integer
  :group 'discourse-graphs-ui)

(defvar dg-ui--http-server nil
  "HTTP server process for serving UI files.")

(defun dg-ui--start-http-server ()
  "Start a simple HTTP server to serve UI files."
  (unless dg-ui--http-server
    (let ((ui-dir (file-name-directory dg-ui-html-file)))
      ;; Try using simple-httpd if available
      (if (require 'simple-httpd nil t)
          (progn
            (setq httpd-root ui-dir)
            (setq httpd-port dg-ui-http-port)
            (httpd-start)
            (setq dg-ui--http-server 'simple-httpd)
            (message "Started simple-httpd server at http://localhost:%d" dg-ui-http-port))
        ;; Fallback to Python http.server
        (let* ((python (or (executable-find "python3")
                          (executable-find "python")))
               (proc (when python
                      (start-process "dg-http-server" "*dg-http-server*"
                                   python "-m" "http.server"
                                   (number-to-string dg-ui-http-port)
                                   "--directory" ui-dir))))
          (if proc
              (progn
                (setq dg-ui--http-server proc)
                (message "Started Python HTTP server at http://localhost:%d" dg-ui-http-port))
            (user-error "Cannot start HTTP server. Install simple-httpd package or ensure Python is available")))))))

(defun dg-ui--stop-http-server ()
  "Stop the HTTP server."
  (when dg-ui--http-server
    (cond
     ((eq dg-ui--http-server 'simple-httpd)
      (when (fboundp 'httpd-stop)
        (httpd-stop))
      (setq dg-ui--http-server nil))
     ((processp dg-ui--http-server)
      (delete-process dg-ui--http-server)
      (setq dg-ui--http-server nil)))
    (message "HTTP server stopped")))

;;;###autoload
(defun dg-ui-open ()
  "Open the discourse graphs UI in browser."
  (interactive)
  ;; Check if HTML file exists
  (unless (file-exists-p dg-ui-html-file)
    (user-error "UI HTML file not found at %s. Did you run 'npm run build'?" dg-ui-html-file))
  
  ;; Start HTTP server if not running
  (unless dg-ui--http-server
    (dg-ui--start-http-server)
    ;; Wait a moment for server to start
    (sleep-for 0.5))
  
  ;; Start WebSocket server if not running
  (unless dg-ui--server
    (message "Starting WebSocket server...")
    (dg-ui-start-server))
  
  ;; Open browser with HTTP URL
  (let ((url (format "http://localhost:%d/index.html" dg-ui-http-port)))
    (message "Opening UI at: %s" url)
    (browse-url url))
  
  ;; Give feedback
  (message "UI opened. WebSocket server: ws://%s:%d (clients: %d)"
           dg-ui-host dg-ui-port (length dg-ui--clients)))

;;;###autoload
(defun dg-ui-toggle ()
  "Toggle the UI server on/off."
  (interactive)
  (if dg-ui--server
      (dg-ui-stop-server)
    (dg-ui-start-server)
    (dg-ui-open)))

;;;###autoload
(defun dg-ui-diagnose ()
  "Diagnose UI setup and display status."
  (interactive)
  (let ((html-exists (file-exists-p dg-ui-html-file))
        (server-running dg-ui--server)
        (node-count (ignore-errors (length (dg-all-nodes))))
        (rel-count (ignore-errors (length (dg-all-relations))))
        (client-count (length dg-ui--clients)))
    
    (with-current-buffer (get-buffer-create "*DG-UI Diagnostics*")
      (erase-buffer)
      (insert "Discourse Graphs UI Diagnostics\n")
      (insert "=================================\n\n")
      
      ;; HTML File
      (insert (format "HTML File: %s\n" dg-ui-html-file))
      (insert (format "  Exists: %s\n" (if html-exists "✓ YES" "✗ NO - Run 'npm run build'")))
      (insert "\n")
      
      ;; WebSocket Server
      (insert (format "WebSocket Server: ws://%s:%d\n" dg-ui-host dg-ui-port))
      (insert (format "  Running: %s\n" (if server-running "✓ YES" "✗ NO")))
      (insert (format "  Clients: %d connected\n" client-count))
      (insert "\n")
      
      ;; Data Functions
      (insert "Data Functions:\n")
      (insert (format "  dg-all-nodes: %s\n" 
                      (if (fboundp 'dg-all-nodes) "✓ defined" "✗ NOT defined")))
      (insert (format "  dg-all-relations: %s\n" 
                      (if (fboundp 'dg-all-relations) "✓ defined" "✗ NOT defined")))
      (insert "\n")
      
      ;; Data
      (insert "Graph Data:\n")
      (insert (format "  Nodes: %s\n" (or node-count "ERROR - cannot retrieve")))
      (insert (format "  Relations: %s\n" (or rel-count "ERROR - cannot retrieve")))
      (insert "\n")
      
      ;; Recommendations
      (insert "Recommendations:\n")
      (unless html-exists
        (insert "  • Run: cd discourse-graphs-ui && npm run build\n"))
      (unless server-running
        (insert "  • Run: M-x dg-ui-start-server\n"))
      (when (and node-count (= node-count 0))
        (insert "  • No nodes in database. Create some nodes first.\n"))
      (when (not (fboundp 'dg-all-relations))
        (insert "  • dg-all-relations function missing. Update discourse-graphs.el\n"))
      
      (goto-char (point-min))
      (special-mode)
      (display-buffer (current-buffer)))))

;;; Hooks

(defun dg-ui--after-save-hook ()
  "Update UI after saving an org file."
  (when (and dg-ui--server
             (derived-mode-p 'org-mode)
             (member (file-name-directory (buffer-file-name)) dg-directories))
    (run-with-timer 0.5 nil #'dg-ui-refresh)))

(defun dg-ui-enable-auto-refresh ()
  "Enable automatic UI refresh on file save."
  (interactive)
  (add-hook 'after-save-hook #'dg-ui--after-save-hook)
  (message "DG-UI: Auto-refresh on save enabled"))

(defun dg-ui-disable-auto-refresh ()
  "Disable automatic UI refresh on file save."
  (interactive)
  (remove-hook 'after-save-hook #'dg-ui--after-save-hook)
  (message "DG-UI: Auto-refresh on save disabled"))

;;; Follow Mode Implementation

(defvar dg-ui--follow-mode nil)

(defvar dg-ui--last-followed-id nil)

(defun dg-ui--follow-hook ()
  (when dg-ui--follow-mode
    (when (and dg-ui--server (derived-mode-p 'org-mode))
      (let ((id (org-entry-get nil "ID")))
        (when (and id (not (string= id dg-ui--last-followed-id))
                   (org-entry-get nil "DG_TYPE"))
          (setq dg-ui--last-followed-id id)
          (let ((msg (json-encode `((type . "focus")
                                    (data . ((id . ,id)))))))
            (dolist (client dg-ui--clients)
              (ignore-errors (websocket-send-text client msg)))))))))

(add-hook 'post-command-hook 'dg-ui--follow-hook)

(defun dg-ui-toggle-follow ()
  (interactive)
  (setq dg-ui--follow-mode (not dg-ui--follow-mode))
  (setq dg-ui--last-followed-id nil)
  (message "Follow Mode %s" (if dg-ui--follow-mode "ON" "OFF")))

;;; Cleanup

;;;###autoload
(defun dg-ui-cleanup ()
  "Clean up UI resources."
  (when dg-ui--server
    (dg-ui-stop-server)
    (message "Web UI stopped"))
  (when dg-ui--follow-mode
    (setq dg-ui--follow-mode nil)
    (setq dg-ui--last-followed-id nil))
  (remove-hook 'post-command-hook 'dg-ui--follow-hook))

(provide 'discourse-graphs-ui)

;;; discourse-graphs-ui.el ends here

