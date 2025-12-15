;;; discourse-graph.el --- Discourse Graph for org-mode with SQLite -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: Your Name
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: org, notes, knowledge-management
;; URL: https://github.com/yourname/discourse-graph

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Discourse Graph is a knowledge synthesis tool for Emacs org-mode.
;; It implements the discourse graph protocol for organizing research notes
;; into semantic units (Questions, Claims, Evidence, Sources) with typed
;; relationships (supports, opposes, answers, etc.).
;;
;; Features:
;; - SQLite-backed storage for scalability
;; - Compatible with denote file naming and linking
;; - Discourse Context sidebar
;; - Computed attributes (support count, evidence score)
;; - Interactive query builder
;; - Export to Graphviz DOT and Markdown
;;
;; Usage:
;;   (require 'discourse-graph)
;;   (setq dg-directories '("~/org/research/"))
;;   (discourse-graph-mode 1)
;;   M-x dg-rebuild-cache

;;; Code:

(require 'org)
(require 'org-element)
(require 'seq)

;;; ============================================================
;;; Custom Group
;;; ============================================================

(defgroup discourse-graph nil
  "Discourse Graph for org-mode knowledge synthesis."
  :group 'org
  :prefix "dg-")

;;; ============================================================
;;; Configuration Variables
;;; ============================================================

(defcustom dg-directories (list org-directory)
  "List of directories to scan for discourse graph nodes."
  :type '(repeat directory)
  :group 'discourse-graph)

(defcustom dg-recursive t
  "Whether to recursively scan subdirectories."
  :type 'boolean
  :group 'discourse-graph)

(defcustom dg-db-file
  (expand-file-name "discourse-graph.db" user-emacs-directory)
  "Path to SQLite database file."
  :type 'file
  :group 'discourse-graph)

(defcustom dg-id-length 8
  "Length of generated hash IDs (when not using denote)."
  :type 'integer
  :group 'discourse-graph)

(defcustom dg-use-denote nil
  "Whether to use denote for file creation and linking.
When non-nil, uses denote's ID format and linking conventions."
  :type 'boolean
  :group 'discourse-graph)

(defcustom dg-denote-keywords-as-type t
  "When using denote, add node type as a keyword in filename.
E.g., 20231215T120000--my-claim__claim.org"
  :type 'boolean
  :group 'discourse-graph)

(defcustom dg-node-types
  '((question . (:short "Q" :color "lightblue"   :desc "Research question"))
    (claim    . (:short "C" :color "lightyellow" :desc "Assertion or thesis"))
    (evidence . (:short "E" :color "lightgreen"  :desc "Supporting data"))
    (source   . (:short "S" :color "lightgray"   :desc "Reference material")))
  "Node types with metadata.
Each entry is (TYPE . (:short ABBREV :color COLOR :desc DESCRIPTION))."
  :type '(alist :key-type symbol
                :value-type (plist :key-type keyword
                                   :value-type string))
  :group 'discourse-graph)

(defcustom dg-relation-types
  '((supports  . (:inverse "Supported By" :color "green"  :style "solid"))
    (opposes   . (:inverse "Opposed By"   :color "red"    :style "dashed"))
    (informs   . (:inverse "Informed By"  :color "blue"   :style "solid"))
    (answers   . (:inverse "Answered By"  :color "purple" :style "solid"))
    (based-on  . (:inverse "Basis For"    :color "orange" :style "solid"))
    (cites     . (:inverse "Cited By"     :color "gray"   :style "dotted")))
  "Relation types with metadata.
Each entry is (TYPE . (:inverse INVERSE-NAME :color COLOR :style STYLE)).
INVERSE-NAME is the human-readable name when viewing from the target's perspective."
  :type '(alist :key-type symbol
                :value-type (plist :key-type keyword
                                   :value-type string))
  :group 'discourse-graph)

(defcustom dg-context-auto-update t
  "Automatically update context buffer when cursor moves to new node."
  :type 'boolean
  :group 'discourse-graph)

(defcustom dg-context-window-width 45
  "Width of the discourse context side window."
  :type 'integer
  :group 'discourse-graph)

(defcustom dg-title-templates
  '((question . "QUE: %s")
    (claim    . "CLM: %s")
    (evidence . "EVD: %s")
    (source   . "SRC: %s"))
  "Title format templates for each node type.
Use %s as placeholder for the actual title.
Set to nil to disable auto-formatting."
  :type '(alist :key-type symbol :value-type string)
  :group 'discourse-graph)

(defcustom dg-auto-format-title nil
  "Whether to automatically format titles using templates."
  :type 'boolean
  :group 'discourse-graph)

(defcustom dg-export-link-style 'wikilink
  "Link style for markdown export.
`wikilink' for [[Title]] style, `markdown' for [Title](file.md) style."
  :type '(choice (const wikilink) (const markdown))
  :group 'discourse-graph)

;;; ============================================================
;;; Internal Variables
;;; ============================================================

(defvar dg--db nil
  "SQLite database connection.")

(defvar dg--context-buffer-name "*DG Context*"
  "Name of the discourse context buffer.")

(defvar dg--index-buffer-name "*DG Index*"
  "Name of the node index buffer.")

(defvar dg--query-buffer-name "*DG Query*"
  "Name of the query results buffer.")

(defvar dg--current-node-id nil
  "ID of currently displayed node in context buffer.")

;;; ============================================================
;;; Database Management
;;; ============================================================

(defun dg--db ()
  "Get database connection, initializing if needed."
  (unless (and dg--db (sqlitep dg--db))
    (setq dg--db (sqlite-open dg-db-file))
    (dg--init-schema))
  dg--db)

(defun dg--init-schema ()
  "Initialize database schema."
  ;; Nodes table
  (sqlite-execute (dg--db) "
    CREATE TABLE IF NOT EXISTS nodes (
      id           TEXT PRIMARY KEY,
      type         TEXT NOT NULL,
      title        TEXT NOT NULL,
      file         TEXT NOT NULL,
      pos          INTEGER NOT NULL,
      outline_path TEXT,
      is_file_node INTEGER DEFAULT 0,
      mtime        REAL NOT NULL
    )")
  ;; Relations table with context support
  (sqlite-execute (dg--db) "
    CREATE TABLE IF NOT EXISTS relations (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      source_id   TEXT NOT NULL,
      target_id   TEXT NOT NULL,
      rel_type    TEXT NOT NULL,
      context_id  TEXT,
      context_note TEXT,
      UNIQUE(source_id, target_id, rel_type)
    )")
  ;; Indexes for performance
  (sqlite-execute (dg--db) "CREATE INDEX IF NOT EXISTS idx_nodes_type ON nodes(type)")
  (sqlite-execute (dg--db) "CREATE INDEX IF NOT EXISTS idx_nodes_file ON nodes(file)")
  (sqlite-execute (dg--db) "CREATE INDEX IF NOT EXISTS idx_rel_source ON relations(source_id)")
  (sqlite-execute (dg--db) "CREATE INDEX IF NOT EXISTS idx_rel_target ON relations(target_id)")
  (sqlite-execute (dg--db) "CREATE INDEX IF NOT EXISTS idx_rel_type ON relations(rel_type)"))

(defun dg-close-db ()
  "Close database connection."
  (interactive)
  (when (and dg--db (sqlitep dg--db))
    (sqlite-close dg--db)
    (setq dg--db nil)
    (message "Discourse Graph: database closed")))

;;; ============================================================
;;; ID Generation
;;; ============================================================

(defun dg-generate-id (&optional content)
  "Generate a unique ID.
If `dg-use-denote' is non-nil and denote is available, use denote format.
Otherwise generate a short hash from CONTENT or random data."
  (if (and dg-use-denote (featurep 'denote))
      ;; Denote-style timestamp ID
      (format-time-string "%Y%m%dT%H%M%S")
    ;; Hash-based ID
    (let* ((input (or content
                      (format "%s-%s-%s"
                              (emacs-pid)
                              (float-time)
                              (random t))))
           (full-hash (secure-hash 'sha256 input)))
      (substring full-hash 0 dg-id-length))))

;;; ============================================================
;;; Denote Compatibility
;;; ============================================================

(defun dg--denote-available-p ()
  "Check if denote is available and enabled."
  (and dg-use-denote (featurep 'denote)))

(defun dg--extract-denote-id (filename)
  "Extract denote ID from FILENAME if present.
Returns the ID string or nil."
  (let ((name (file-name-nondirectory filename)))
    (when (string-match "^\\([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]T[0-9][0-9][0-9][0-9][0-9][0-9]\\)" name)
      (match-string 1 name))))

(defun dg--get-id-at-point ()
  "Get discourse graph node ID at point.
Checks org ID property first, then denote identifier, then file-based denote ID."
  (or (org-entry-get nil "ID")
      (org-entry-get nil "IDENTIFIER")
      ;; Check file-level for denote
      (when (buffer-file-name)
        (dg--extract-denote-id (buffer-file-name)))))

(defun dg--create-denote-node (type title)
  "Create a new denote file for a discourse graph node.
TYPE is the node type symbol, TITLE is the node title string."
  (when (dg--denote-available-p)
    (let* ((keywords (if dg-denote-keywords-as-type
                         (list (symbol-name type))
                       nil))
           (denote-directory (car dg-directories)))
      (denote title keywords 'org)
      ;; Add DG_TYPE to front matter
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+identifier:" nil t)
          (forward-line 1)
          (insert (format "#+dg_type: %s\n" type)))))))

;;; ============================================================
;;; File Collection and Scanning
;;; ============================================================

(defun dg--collect-files ()
  "Collect all org files from configured directories."
  (let ((files '()))
    (dolist (dir dg-directories)
      (when (file-directory-p dir)
        (setq files
              (append files
                      (if dg-recursive
                          (directory-files-recursively dir "\\.org$")
                        (directory-files dir t "\\.org$"))))))
    (seq-uniq files)))

(defun dg--parse-relations-at-point ()
  "Parse all DG relation properties at current heading."
  (let ((relations '()))
    (dolist (rel-type dg-relation-types)
      (let* ((prop (concat "DG_" (upcase (symbol-name (car rel-type)))))
             (value (org-entry-get nil prop)))
        (when value
          ;; Support multiple targets separated by space or comma
          (dolist (target (split-string value "[ \t,]+" t))
            (push (cons (car rel-type) target) relations)))))
    relations))

(defun dg--get-node-type-at-point ()
  "Get DG_TYPE at point, checking both property and front matter."
  (or (org-entry-get nil "DG_TYPE")
      ;; Check org-mode keywords for denote compatibility
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+dg_type:[ \t]*\\(.+\\)" nil t)
          (string-trim (match-string 1))))))

(defun dg--scan-file (file)
  "Scan FILE for discourse graph nodes.
Returns (NODES . RELATIONS) where each is a list."
  (let ((nodes '())
        (relations '())
        (file-mtime (float-time (file-attribute-modification-time
                                 (file-attributes file))))
        (existing-buffer (get-file-buffer file)))
    (condition-case err
        (let ((buf (or existing-buffer
                       (let ((inhibit-message t)
                             (message-log-max nil))
                         (find-file-noselect file t)))))
          (with-current-buffer buf
            (save-excursion
              (save-restriction
                (widen)
                ;; Check for file-level node (denote style)
                (let* ((denote-id (dg--extract-denote-id file))
                       (file-type (progn
                                    (goto-char (point-min))
                                    (when (re-search-forward "^#\\+dg_type:[ \t]*\\(.+\\)" nil t)
                                      (string-trim (match-string 1)))))
                       (file-title (progn
                                     (goto-char (point-min))
                                     (when (re-search-forward "^#\\+title:[ \t]*\\(.+\\)" nil t)
                                       (string-trim (match-string 1))))))
                  (when (and denote-id file-type file-title)
                    (push (list :id denote-id
                                :type file-type
                                :title file-title
                                :file file
                                :pos 1
                                :outline-path ""
                                :is-file-node t
                                :mtime file-mtime)
                          nodes)
                    ;; Parse file-level relations from keywords
                    (goto-char (point-min))
                    (while (re-search-forward "^#\\+dg_\\([a-z_-]+\\):[ \t]*\\(.+\\)" nil t)
                      (let ((rel-name (match-string 1))
                            (targets (match-string 2)))
                        (unless (string= rel-name "type")
                          (dolist (target (split-string targets "[ \t,]+" t))
                            (push (list :source denote-id
                                        :target target
                                        :type (intern (replace-regexp-in-string "_" "-" rel-name)))
                                  relations)))))))
                ;; Scan headings
                (goto-char (point-min))
                (while (re-search-forward "^\\*+ " nil t)
                  (condition-case nil
                      (save-excursion
                        (beginning-of-line)
                        (let ((id (org-entry-get nil "ID"))
                              (type (org-entry-get nil "DG_TYPE")))
                          (when (and id type)
                            (push (list :id id
                                        :type type
                                        :title (org-get-heading t t t t)
                                        :file file
                                        :pos (point)
                                        :outline-path (ignore-errors
                                                        (string-join (org-get-outline-path t) "/"))
                                        :is-file-node nil
                                        :mtime file-mtime)
                                  nodes)
                            (dolist (rel (dg--parse-relations-at-point))
                              (push (list :source id
                                          :target (cdr rel)
                                          :type (car rel))
                                    relations)))))
                    (error nil))))))
          ;; Kill buffer if we opened it
          (unless existing-buffer
            (kill-buffer buf)))
      (error
       (message "DG: Error scanning %s: %s" file (error-message-string err))))
    (cons nodes relations)))

;;; ============================================================
;;; Database Operations
;;; ============================================================

(defun dg--save-node (node)
  "Save NODE to database."
  (sqlite-execute
   (dg--db)
   "INSERT OR REPLACE INTO nodes
    (id, type, title, file, pos, outline_path, is_file_node, mtime)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
   (list (plist-get node :id)
         (plist-get node :type)
         (plist-get node :title)
         (plist-get node :file)
         (plist-get node :pos)
         (plist-get node :outline-path)
         (if (plist-get node :is-file-node) 1 0)
         (plist-get node :mtime))))

(defun dg--save-relation (rel)
  "Save REL to database."
  (sqlite-execute
   (dg--db)
   "INSERT OR IGNORE INTO relations (source_id, target_id, rel_type)
    VALUES (?, ?, ?)"
   (list (plist-get rel :source)
         (plist-get rel :target)
         (symbol-name (plist-get rel :type)))))

(defun dg--delete-file-data (file)
  "Delete all nodes and relations from FILE."
  (sqlite-execute
   (dg--db)
   "DELETE FROM relations WHERE source_id IN
    (SELECT id FROM nodes WHERE file = ?)"
   (list file))
  (sqlite-execute
   (dg--db)
   "DELETE FROM nodes WHERE file = ?"
   (list file)))

;;; ============================================================
;;; Cache Management
;;; ============================================================

(defun dg-rebuild-cache ()
  "Completely rebuild the database from all configured directories."
  (interactive)
  (let ((inhibit-message t)
        (message-log-max nil))
    (sqlite-execute (dg--db) "DELETE FROM relations")
    (sqlite-execute (dg--db) "DELETE FROM nodes"))
  (let ((files (dg--collect-files))
        (node-count 0)
        (rel-count 0)
        (file-count 0)
        (total-files 0))
    (setq total-files (length files))
    (message "Discourse Graph: scanning %d files..." total-files)
    (dolist (file files)
      (cl-incf file-count)
      ;; Show progress every 10 files
      (when (= 0 (mod file-count 10))
        (message "Discourse Graph: scanning... %d/%d" file-count total-files))
      (let* ((inhibit-message t)
             (message-log-max nil)
             (result (dg--scan-file file))
             (nodes (car result))
             (relations (cdr result)))
        (dolist (node nodes)
          (dg--save-node node)
          (cl-incf node-count))
        (dolist (rel relations)
          (dg--save-relation rel)
          (cl-incf rel-count))))
    (message "Discourse Graph: indexed %d nodes, %d relations from %d files"
             node-count rel-count total-files)))

(defun dg-update-file (&optional file)
  "Incrementally update index for FILE (defaults to current buffer)."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (when (and file
               (file-exists-p file)
               (string-suffix-p ".org" file))
      (condition-case err
          (let ((inhibit-message t)
                (message-log-max nil))
            (dg--delete-file-data file)
            (let* ((result (dg--scan-file file))
                   (nodes (car result))
                   (relations (cdr result)))
              (dolist (node nodes)
                (dg--save-node node))
              (dolist (rel relations)
                (dg--save-relation rel))
              ;; Only show message if called interactively
              (when (called-interactively-p 'any)
                (let ((inhibit-message nil))
                  (message "Discourse Graph: updated %d nodes from %s"
                           (length nodes) (file-name-nondirectory file))))))
        (error
         (message "DG: Error updating %s: %s" file (error-message-string err)))))))

;;; ============================================================
;;; Query API
;;; ============================================================

(defun dg--row-to-plist (row)
  "Convert database ROW to property list."
  (when row
    (list :id (nth 0 row)
          :type (intern (nth 1 row))
          :title (nth 2 row)
          :file (nth 3 row)
          :pos (nth 4 row)
          :outline-path (nth 5 row))))

(defun dg-get (id)
  "Get node by ID."
  (dg--row-to-plist
   (car (sqlite-select
         (dg--db)
         "SELECT id, type, title, file, pos, outline_path
          FROM nodes WHERE id = ?"
         (list id)))))

(defun dg-find-by-type (type)
  "Find all nodes of TYPE."
  (mapcar #'dg--row-to-plist
          (sqlite-select
           (dg--db)
           "SELECT id, type, title, file, pos, outline_path
            FROM nodes WHERE type = ? ORDER BY title"
           (list (if (symbolp type) (symbol-name type) type)))))

(defun dg-find-by-title (pattern)
  "Find nodes with titles matching PATTERN."
  (mapcar #'dg--row-to-plist
          (sqlite-select
           (dg--db)
           "SELECT id, type, title, file, pos, outline_path
            FROM nodes WHERE title LIKE ? ORDER BY title"
           (list (concat "%" pattern "%")))))

(defun dg-find-outgoing (id &optional rel-type)
  "Find nodes that ID points to, optionally filtered by REL-TYPE."
  (let ((sql (if rel-type
                 "SELECT n.id, n.type, n.title, n.file, n.pos, n.outline_path
                  FROM nodes n
                  JOIN relations r ON n.id = r.target_id
                  WHERE r.source_id = ? AND r.rel_type = ?"
               "SELECT n.id, n.type, n.title, n.file, n.pos, n.outline_path
                FROM nodes n
                JOIN relations r ON n.id = r.target_id
                WHERE r.source_id = ?"))
        (params (if rel-type
                    (list id (symbol-name rel-type))
                  (list id))))
    (mapcar #'dg--row-to-plist
            (sqlite-select (dg--db) sql params))))

(defun dg-find-incoming (id &optional rel-type)
  "Find nodes that point to ID, optionally filtered by REL-TYPE."
  (let ((sql (if rel-type
                 "SELECT n.id, n.type, n.title, n.file, n.pos, n.outline_path
                  FROM nodes n
                  JOIN relations r ON n.id = r.source_id
                  WHERE r.target_id = ? AND r.rel_type = ?"
               "SELECT n.id, n.type, n.title, n.file, n.pos, n.outline_path
                FROM nodes n
                JOIN relations r ON n.id = r.source_id
                WHERE r.target_id = ?"))
        (params (if rel-type
                    (list id (symbol-name rel-type))
                  (list id))))
    (mapcar #'dg--row-to-plist
            (sqlite-select (dg--db) sql params))))

(defun dg-get-relations (id)
  "Get all relations for node ID.
Returns plist with :outgoing and :incoming lists."
  (let ((outgoing (sqlite-select
                   (dg--db)
                   "SELECT 'out', rel_type, target_id, n.title, n.type
                    FROM relations r
                    LEFT JOIN nodes n ON r.target_id = n.id
                    WHERE r.source_id = ?"
                   (list id)))
        (incoming (sqlite-select
                   (dg--db)
                   "SELECT 'in', rel_type, source_id, n.title, n.type
                    FROM relations r
                    LEFT JOIN nodes n ON r.source_id = n.id
                    WHERE r.target_id = ?"
                   (list id))))
    (list :outgoing outgoing :incoming incoming)))

(defun dg-all-nodes ()
  "Get all nodes."
  (mapcar #'dg--row-to-plist
          (sqlite-select
           (dg--db)
           "SELECT id, type, title, file, pos, outline_path
            FROM nodes ORDER BY type, title")))

;;; ============================================================
;;; Convenience Query Functions
;;; ============================================================

(defun dg-find-answers (question-id)
  "Find claims that answer QUESTION-ID."
  (dg-find-incoming question-id 'answers))

(defun dg-find-supporting-evidence (claim-id)
  "Find evidence that supports CLAIM-ID."
  (dg-find-incoming claim-id 'supports))

(defun dg-find-opposing-evidence (claim-id)
  "Find evidence that opposes CLAIM-ID."
  (dg-find-incoming claim-id 'opposes))

(defun dg-find-sources-for (evidence-id)
  "Find sources cited by EVIDENCE-ID."
  (dg-find-outgoing evidence-id 'cites))

;;; ============================================================
;;; Discourse Attributes (Computed Properties)
;;; ============================================================

(defun dg-attr-support-count (id)
  "Count nodes that support ID."
  (or (caar (sqlite-select
             (dg--db)
             "SELECT COUNT(*) FROM relations
              WHERE target_id = ? AND rel_type = 'supports'"
             (list id)))
      0))

(defun dg-attr-oppose-count (id)
  "Count nodes that oppose ID."
  (or (caar (sqlite-select
             (dg--db)
             "SELECT COUNT(*) FROM relations
              WHERE target_id = ? AND rel_type = 'opposes'"
             (list id)))
      0))

(defun dg-attr-evidence-score (id)
  "Calculate evidence score: supports - opposes."
  (- (dg-attr-support-count id)
     (dg-attr-oppose-count id)))

(defun dg-attr-citation-count (id)
  "Count citations to ID."
  (or (caar (sqlite-select
             (dg--db)
             "SELECT COUNT(*) FROM relations
              WHERE target_id = ? AND rel_type = 'cites'"
             (list id)))
      0))

(defun dg-attr-answer-count (id)
  "Count answers to question ID."
  (or (caar (sqlite-select
             (dg--db)
             "SELECT COUNT(*) FROM relations
              WHERE target_id = ? AND rel_type = 'answers'"
             (list id)))
      0))

(defun dg-get-all-attributes (id)
  "Get all computed attributes for ID."
  (list :support-count (dg-attr-support-count id)
        :oppose-count (dg-attr-oppose-count id)
        :evidence-score (dg-attr-evidence-score id)
        :citation-count (dg-attr-citation-count id)
        :answer-count (dg-attr-answer-count id)))

;;; ============================================================
;;; Statistics
;;; ============================================================

(defun dg-stats ()
  "Display discourse graph statistics."
  (interactive)
  (let ((node-count (caar (sqlite-select (dg--db) "SELECT COUNT(*) FROM nodes")))
        (rel-count (caar (sqlite-select (dg--db) "SELECT COUNT(*) FROM relations")))
        (type-stats (sqlite-select (dg--db)
                                   "SELECT type, COUNT(*) FROM nodes GROUP BY type ORDER BY type"))
        (rel-stats (sqlite-select (dg--db)
                                  "SELECT rel_type, COUNT(*) FROM relations GROUP BY rel_type ORDER BY rel_type"))
        (file-count (caar (sqlite-select (dg--db)
                                         "SELECT COUNT(DISTINCT file) FROM nodes"))))
    (with-current-buffer (get-buffer-create "*DG Stats*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Discourse Graph Statistics\n"
                            'face '(:weight bold :height 1.3)))
        (insert (make-string 40 ?═) "\n\n")
        (insert (format "Total nodes:     %d\n" node-count))
        (insert (format "Total relations: %d\n" rel-count))
        (insert (format "Files indexed:   %d\n\n" file-count))
        (insert (propertize "Nodes by Type\n" 'face '(:weight bold)))
        (insert (make-string 20 ?─) "\n")
        (dolist (row type-stats)
          (let* ((type (intern (car row)))
                 (count (cadr row))
                 (info (alist-get type dg-node-types)))
            (insert (format "  %s %-10s %4d\n"
                            (plist-get info :short)
                            (car row)
                            count))))
        (insert (format "\n"))
        (insert (propertize "Relations by Type\n" 'face '(:weight bold)))
        (insert (make-string 20 ?─) "\n")
        (dolist (row rel-stats)
          (insert (format "  %-12s %4d\n" (car row) (cadr row))))
        (goto-char (point-min)))
      (special-mode)
      (display-buffer (current-buffer)))))

;;; ============================================================
;;; Discourse Context Panel
;;; ============================================================

(defun dg-context-refresh ()
  "Refresh discourse context for node at point."
  (interactive)
  (let ((id (dg--get-id-at-point)))
    (if id
        (dg--display-context id)
      (message "No discourse graph node at point"))))

(defun dg--display-context (id)
  "Display discourse context for node ID in side window."
  (when (not (equal id dg--current-node-id))
    (setq dg--current-node-id id)
    (let* ((node (dg-get id))
           (rels (dg-get-relations id))
           (attrs (dg-get-all-attributes id))
           (outgoing (plist-get rels :outgoing))
           (incoming (plist-get rels :incoming)))
      (with-current-buffer (get-buffer-create dg--context-buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; Header - use org-document-title face
          (let* ((type (plist-get node :type))
                 (type-info (alist-get type dg-node-types))
                 (short (plist-get type-info :short)))
            (insert (propertize (format "[%s] %s\n"
                                        short
                                        (or (plist-get node :title) "Unknown"))
                                'face 'org-document-title))
            (insert (propertize (format "ID: %s\n" id)
                                'face 'font-lock-comment-face)))

          ;; Attributes - more explicit labels
          (let ((supp (plist-get attrs :support-count))
                (opp (plist-get attrs :oppose-count))
                (score (plist-get attrs :evidence-score))
                (ans (plist-get attrs :answer-count))
                (cit (plist-get attrs :citation-count)))
            (insert "\n")
            (when (or (> supp 0) (> opp 0))
              (insert (format "  %d supporting, %d opposing (net %+d)\n" supp opp score)))
            (when (> ans 0)
              (insert (format "  %d answers\n" ans)))
            (when (> cit 0)
              (insert (format "  Cited %d times\n" cit))))

          ;; Outgoing relations - use org-level-1 face
          (when outgoing
            (insert "\n")
            (let ((grouped (seq-group-by (lambda (r) (nth 1 r)) outgoing)))
              (dolist (group grouped)
                (let* ((rel-type (intern (car group)))
                       (display-name (capitalize (symbol-name rel-type))))
                  (insert (propertize (format "%s\n" display-name)
                                      'face 'org-level-1))
                  (dolist (r (cdr group))
                    (let ((target-id (nth 2 r))
                          (target-title (or (nth 3 r) "?"))
                          (target-type (nth 4 r)))
                      (insert "  ")
                      (insert-text-button
                       (truncate-string-to-width target-title 35 nil nil "…")
                       'action (lambda (_) (dg-goto-node-by-id target-id))
                       'target-id target-id
                       'face 'link)
                      (when target-type
                        (insert (propertize (format " [%s]"
                                                    (plist-get (alist-get (intern target-type) dg-node-types) :short))
                                            'face 'font-lock-comment-face)))
                      (insert "\n")))))))

          ;; Incoming relations - use org-level-2 face
          (when incoming
            (insert "\n")
            (let ((grouped (seq-group-by (lambda (r) (nth 1 r)) incoming)))
              (dolist (group grouped)
                (let* ((rel-type (intern (car group)))
                       (rel-info (alist-get rel-type dg-relation-types))
                       (inverse-name (or (plist-get rel-info :inverse)
                                         (format "%s (inverse)" (car group)))))
                  (insert (propertize (format "%s\n" inverse-name)
                                      'face 'org-level-2))
                  (dolist (r (cdr group))
                    (let ((source-id (nth 2 r))
                          (source-title (or (nth 3 r) "?"))
                          (source-type (nth 4 r)))
                      (insert "  ")
                      (insert-text-button
                       (truncate-string-to-width source-title 35 nil nil "…")
                       'action (lambda (_) (dg-goto-node-by-id source-id))
                       'source-id source-id
                       'face 'link)
                      (when source-type
                        (insert (propertize (format " [%s]"
                                                    (plist-get (alist-get (intern source-type) dg-node-types) :short))
                                            'face 'font-lock-comment-face)))
                      (insert "\n")))))))

          (goto-char (point-min)))
        (dg-context-mode)
        (display-buffer (current-buffer)
                        `(display-buffer-in-side-window
                          (side . right)
                          (window-width . ,dg-context-window-width)))))))

(defun dg-context-toggle ()
  "Toggle discourse context side window."
  (interactive)
  (let ((win (get-buffer-window dg--context-buffer-name)))
    (if win
        (delete-window win)
      (dg-context-refresh))))

(define-derived-mode dg-context-mode special-mode "DG-Context"
  "Major mode for discourse graph context display."
  (setq-local cursor-type nil))

;;; ============================================================
;;; Node Index
;;; ============================================================

(defun dg-node-index (&optional type sort-by)
  "Display index of nodes, optionally filtered by TYPE and sorted by SORT-BY."
  (interactive
   (list (when current-prefix-arg
           (intern (completing-read "Type (empty for all): "
                                    (cons "" (mapcar #'car dg-node-types))
                                    nil nil)))
         (intern (completing-read "Sort by: "
                                  '("title" "evidence-score" "support-count" "type")
                                  nil nil "title"))))
  (let* ((nodes (if (and type (not (string-empty-p (symbol-name type))))
                    (dg-find-by-type type)
                  (dg-all-nodes)))
         ;; Add attributes
         (nodes-with-attrs
          (mapcar (lambda (n)
                    (let ((id (plist-get n :id)))
                      (append n (list :attrs (dg-get-all-attributes id)))))
                  nodes))
         ;; Sort
         (sorted
          (pcase sort-by
            ('title
             (seq-sort-by (lambda (n) (downcase (plist-get n :title))) #'string< nodes-with-attrs))
            ('evidence-score
             (seq-sort-by (lambda (n) (plist-get (plist-get n :attrs) :evidence-score))
                          #'> nodes-with-attrs))
            ('support-count
             (seq-sort-by (lambda (n) (plist-get (plist-get n :attrs) :support-count))
                          #'> nodes-with-attrs))
            ('type
             (seq-sort-by (lambda (n) (symbol-name (plist-get n :type))) #'string< nodes-with-attrs))
            (_ nodes-with-attrs))))
    (with-current-buffer (get-buffer-create dg--index-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Node Index%s (%d nodes)\n"
                                    (if type (format " [%s]" type) "")
                                    (length sorted))
                            'face '(:weight bold :height 1.2)))
        (insert (make-string 70 ?═) "\n")
        ;; Table header
        (insert (propertize
                 (format "%-3s %-45s %5s %5s %5s\n"
                         "T" "Title" "Supp" "Opp" "Score")
                 'face '(:weight bold)))
        (insert (make-string 70 ?─) "\n")
        ;; Rows
        (dolist (node sorted)
          (let* ((id (plist-get node :id))
                 (ntype (plist-get node :type))
                 (type-info (alist-get ntype dg-node-types))
                 (short (plist-get type-info :short))
                 (title (truncate-string-to-width
                         (plist-get node :title) 43 nil nil "…"))
                 (attrs (plist-get node :attrs))
                 (supp (plist-get attrs :support-count))
                 (opp (plist-get attrs :oppose-count))
                 (score (plist-get attrs :evidence-score)))
            (insert (format "%-3s " short))
            (insert-text-button
             (format "%-45s" title)
             'action (lambda (_) (dg-goto-node-by-id id))
             'node-id id
             'face 'link)
            (insert (format " %5d %5d %+5d\n" supp opp score))))
        (goto-char (point-min)))
      (dg-index-mode)
      (display-buffer (current-buffer)))))

(define-derived-mode dg-index-mode special-mode "DG-Index"
  "Major mode for discourse graph node index."
  (setq-local truncate-lines t))

;;; ============================================================
;;; Query Builder
;;; ============================================================

(defun dg-query-builder ()
  "Interactive query builder for discourse graph."
  (interactive)
  (let* ((source-type (intern (completing-read
                               "Find nodes of type: "
                               (mapcar #'car dg-node-types))))
         (add-rel (y-or-n-p "Add relation filter? "))
         (rel-type (when add-rel
                     (intern (completing-read
                              "With relation: "
                              (mapcar #'car dg-relation-types)))))
         (rel-dir (when add-rel
                    (intern (completing-read
                             "Direction: "
                             '("outgoing" "incoming")))))
         (target-type (when add-rel
                        (let ((sel (completing-read
                                    "To/from type (empty for any): "
                                    (cons "" (mapcar (lambda (x) (symbol-name (car x)))
                                                     dg-node-types)))))
                          (unless (string-empty-p sel)
                            (intern sel)))))
         (results (dg--execute-query source-type rel-type rel-dir target-type)))
    (dg--display-query-results results
                               (format "%s %s %s %s"
                                       source-type
                                       (or rel-type "")
                                       (or rel-dir "")
                                       (or target-type "")))))

(defun dg--execute-query (source-type &optional rel-type rel-dir target-type)
  "Execute query with given parameters."
  (let ((sql "SELECT DISTINCT n.id, n.type, n.title, n.file, n.pos, n.outline_path
              FROM nodes n")
        (conditions (list (format "n.type = '%s'" source-type)))
        (joins ""))
    (when rel-type
      (pcase rel-dir
        ('outgoing
         (setq joins " JOIN relations r ON n.id = r.source_id")
         (push (format "r.rel_type = '%s'" rel-type) conditions)
         (when target-type
           (setq joins (concat joins " JOIN nodes n2 ON r.target_id = n2.id"))
           (push (format "n2.type = '%s'" target-type) conditions)))
        ('incoming
         (setq joins " JOIN relations r ON n.id = r.target_id")
         (push (format "r.rel_type = '%s'" rel-type) conditions)
         (when target-type
           (setq joins (concat joins " JOIN nodes n2 ON r.source_id = n2.id"))
           (push (format "n2.type = '%s'" target-type) conditions)))))
    (let ((full-sql (format "%s%s WHERE %s ORDER BY n.title"
                            sql joins
                            (string-join conditions " AND "))))
      (mapcar #'dg--row-to-plist
              (sqlite-select (dg--db) full-sql)))))

(defun dg--display-query-results (results query-desc)
  "Display RESULTS in query buffer with QUERY-DESC description."
  (with-current-buffer (get-buffer-create dg--query-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Query Results\n" 'face '(:weight bold :height 1.2)))
      (insert (propertize (format "Query: %s\n" query-desc)
                          'face 'font-lock-comment-face))
      (insert (format "Found: %d nodes\n" (length results)))
      (insert (make-string 50 ?─) "\n\n")
      (if (null results)
          (insert (propertize "No results found.\n" 'face 'font-lock-warning-face))
        (dolist (node results)
          (let* ((id (plist-get node :id))
                 (ntype (plist-get node :type))
                 (type-info (alist-get ntype dg-node-types))
                 (short (plist-get type-info :short)))
            (insert (format "[%s] " short))
            (insert-text-button
             (plist-get node :title)
             'action (lambda (_) (dg-goto-node-by-id id))
             'node-id id
             'face 'link)
            (insert "\n"))))
      (goto-char (point-min)))
    (special-mode)
    (display-buffer (current-buffer))))

;;; ============================================================
;;; Node Navigation
;;; ============================================================

(defun dg--completing-read-node (prompt &optional type)
  "Interactively select a node with PROMPT, optionally filtered by TYPE."
  (let* ((sql (if type
                  "SELECT id, type, title FROM nodes WHERE type = ? ORDER BY title"
                "SELECT id, type, title FROM nodes ORDER BY type, title"))
         (params (when type (list (symbol-name type))))
         (rows (sqlite-select (dg--db) sql params))
         (candidates (mapcar (lambda (row)
                               (let* ((ntype (intern (nth 1 row)))
                                      (type-info (alist-get ntype dg-node-types))
                                      (short (plist-get type-info :short)))
                                 (cons (format "[%s] %s" short (nth 2 row))
                                       (nth 0 row))))
                             rows)))
    (when candidates
      (alist-get (completing-read prompt candidates) candidates nil nil #'equal))))

(defun dg-goto-node ()
  "Jump to a discourse graph node."
  (interactive)
  (let ((id (dg--completing-read-node "Go to node: ")))
    (when id
      (dg-goto-node-by-id id))))

(defun dg-goto-node-by-id (id)
  "Jump to node with ID."
  (let ((node (dg-get id)))
    (when node
      (find-file (plist-get node :file))
      (goto-char (plist-get node :pos))
      (org-reveal)
      (org-show-entry)
      (when dg-context-auto-update
        (run-with-idle-timer 0.1 nil #'dg-context-refresh)))))

;;; ============================================================
;;; Node Creation
;;; ============================================================

(defun dg--format-title (type title)
  "Format TITLE according to TYPE template if enabled."
  (if (and dg-auto-format-title dg-title-templates)
      (let ((template (alist-get type dg-title-templates)))
        (if template
            (format template title)
          title))
    title))

(defun dg-create-node (type title)
  "Create a new discourse graph node of TYPE with TITLE."
  (interactive
   (list (intern (completing-read "Type: " (mapcar #'car dg-node-types)))
         (read-string "Title: ")))
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be in org-mode buffer to create node"))
  (if (dg--denote-available-p)
      ;; Use denote to create file-level node
      (dg--create-denote-node type title)
    ;; Create heading-level node
    (let ((id (dg-generate-id (format "%s-%s-%s" type title (float-time))))
          (formatted-title (dg--format-title type title)))
      (org-insert-heading-respect-content)
      (insert formatted-title)
      (org-set-property "ID" id)
      (org-set-property "DG_TYPE" (symbol-name type))
      (when (buffer-file-name)
        (save-buffer)
        (dg-update-file))
      (message "Created [%s] node: %s" type id)
      id)))

(defun dg-create-question (title)
  "Create a question node with TITLE."
  (interactive "sQuestion: ")
  (dg-create-node 'question title))

(defun dg-create-claim (title)
  "Create a claim node with TITLE."
  (interactive "sClaim: ")
  (dg-create-node 'claim title))

(defun dg-create-evidence (title)
  "Create an evidence node with TITLE."
  (interactive "sEvidence: ")
  (dg-create-node 'evidence title))

(defun dg-create-source (title)
  "Create a source node with TITLE."
  (interactive "sSource: ")
  (dg-create-node 'source title))

;;; ============================================================
;;; Relation Management
;;; ============================================================

(defun dg-add-relation (rel-type target-id)
  "Add a relation of REL-TYPE to TARGET-ID from current node."
  (interactive
   (list (intern (completing-read "Relation: "
                                  (mapcar #'car dg-relation-types)))
         (dg--completing-read-node "Target: ")))
  (let* ((prop (concat "DG_" (upcase (symbol-name rel-type))))
         (existing (org-entry-get nil prop)))
    (org-set-property prop
                      (if existing
                          (concat existing " " target-id)
                        target-id))
    (dg-update-file)
    (message "Added relation: %s → %s" rel-type target-id)))

(defun dg-add-relation-with-context (rel-type target-id context-note)
  "Add relation with context.
REL-TYPE is the relation, TARGET-ID is the target node,
CONTEXT-NOTE is an optional note about the relation."
  (interactive
   (list (intern (completing-read "Relation: "
                                  (mapcar #'car dg-relation-types)))
         (dg--completing-read-node "Target: ")
         (read-string "Context note (optional): ")))
  (let* ((source-id (dg--get-id-at-point))
         (prop (concat "DG_" (upcase (symbol-name rel-type))))
         (existing (org-entry-get nil prop)))
    (unless source-id
      (user-error "No discourse graph node at point"))
    ;; Update property
    (org-set-property prop
                      (if existing
                          (concat existing " " target-id)
                        target-id))
    ;; Store with context in database
    (sqlite-execute
     (dg--db)
     "INSERT OR REPLACE INTO relations (source_id, target_id, rel_type, context_note)
      VALUES (?, ?, ?, ?)"
     (list source-id target-id (symbol-name rel-type)
           (unless (string-empty-p context-note) context-note)))
    (message "Added relation with context: %s → %s" rel-type target-id)))

;;; ============================================================
;;; Export: Graphviz DOT
;;; ============================================================

(defun dg-export-dot (&optional file)
  "Export discourse graph to Graphviz DOT format.
If FILE is nil, prompt for output path."
  (interactive "FExport to DOT file: ")
  (let ((nodes (sqlite-select (dg--db) "SELECT id, type, title FROM nodes"))
        (relations (sqlite-select (dg--db) "SELECT source_id, target_id, rel_type FROM relations")))
    (with-temp-file file
      (insert "digraph DiscourseGraph {\n")
      (insert "  rankdir=LR;\n")
      (insert "  node [shape=box, style=filled, fontname=\"Helvetica\"];\n")
      (insert "  edge [fontname=\"Helvetica\", fontsize=10];\n\n")
      ;; Nodes with colors by type
      (insert "  // Nodes\n")
      (dolist (node nodes)
        (let* ((id (nth 0 node))
               (ntype (intern (nth 1 node)))
               (title (nth 2 node))
               (type-info (alist-get ntype dg-node-types))
               (color (or (plist-get type-info :color) "white"))
               (safe-title (replace-regexp-in-string "\"" "\\\\\""
                           (truncate-string-to-width title 40 nil nil "..."))))
          (insert (format "  \"%s\" [label=\"%s\", fillcolor=\"%s\"];\n"
                          id safe-title color))))
      (insert "\n  // Relations\n")
      ;; Relations with colors and styles
      (dolist (rel relations)
        (let* ((rel-type (intern (nth 2 rel)))
               (rel-info (alist-get rel-type dg-relation-types))
               (color (or (plist-get rel-info :color) "black"))
               (style (or (plist-get rel-info :style) "solid")))
          (insert (format "  \"%s\" -> \"%s\" [label=\"%s\", color=\"%s\", style=\"%s\"];\n"
                          (nth 0 rel) (nth 1 rel) (nth 2 rel) color style))))
      (insert "}\n"))
    (message "Exported to %s" file)))

;;; ============================================================
;;; Export: Markdown
;;; ============================================================

(defun dg--sanitize-filename (title)
  "Sanitize TITLE for use as filename."
  (let ((clean (replace-regexp-in-string "[\\/:*?\"<>|]" "_" title)))
    (truncate-string-to-width clean 60 nil nil)))

(defun dg--format-md-link (title)
  "Format markdown link to TITLE."
  (pcase dg-export-link-style
    ('wikilink (format "[[%s]]" title))
    ('markdown (format "[%s](%s.md)" title (dg--sanitize-filename title)))))

(defun dg-export-markdown (&optional directory)
  "Export entire discourse graph to markdown files in DIRECTORY."
  (interactive "DExport to directory: ")
  (let ((nodes (sqlite-select (dg--db)
                              "SELECT id, type, title, outline_path FROM nodes"))
        (exported 0))
    (dolist (row nodes)
      (let* ((id (nth 0 row))
             (ntype (nth 1 row))
             (title (nth 2 row))
             (filename (dg--sanitize-filename title))
             (filepath (expand-file-name (concat filename ".md") directory))
             (rels (dg-get-relations id))
             (attrs (dg-get-all-attributes id)))
        (with-temp-file filepath
          ;; YAML front matter
          (insert "---\n")
          (insert (format "title: \"%s\"\n" (replace-regexp-in-string "\"" "\\\\\"" title)))
          (insert (format "type: %s\n" ntype))
          (insert (format "id: %s\n" id))
          (insert "---\n\n")
          ;; Title
          (insert (format "# %s\n\n" title))
          ;; Attributes
          (insert "## Attributes\n\n")
          (insert (format "- **Type**: %s\n" ntype))
          (insert (format "- **Support Score**: %+d (↑%d ↓%d)\n"
                          (plist-get attrs :evidence-score)
                          (plist-get attrs :support-count)
                          (plist-get attrs :oppose-count)))
          ;; Relations
          (when (plist-get rels :outgoing)
            (insert "\n## Outgoing Relations\n\n")
            (let ((grouped (seq-group-by (lambda (r) (nth 1 r))
                                         (plist-get rels :outgoing))))
              (dolist (group grouped)
                (insert (format "### %s\n\n" (car group)))
                (dolist (r (cdr group))
                  (let ((target-title (or (nth 3 r) (nth 2 r))))
                    (insert (format "- %s\n" (dg--format-md-link target-title))))))))
          (when (plist-get rels :incoming)
            (insert "\n## Incoming Relations\n\n")
            (let ((grouped (seq-group-by (lambda (r) (nth 1 r))
                                         (plist-get rels :incoming))))
              (dolist (group grouped)
                (insert (format "### %s\n\n" (car group)))
                (dolist (r (cdr group))
                  (let ((source-title (or (nth 3 r) (nth 2 r))))
                    (insert (format "- %s\n" (dg--format-md-link source-title)))))))))
        (cl-incf exported)))
    (message "Exported %d nodes to %s" exported directory)))

;;; ============================================================
;;; Hooks and Auto-update
;;; ============================================================

(defun dg--after-save-hook ()
  "Hook to update index after saving org file."
  (when (and discourse-graph-mode
             (derived-mode-p 'org-mode)
             (buffer-file-name))
    (dg-update-file)))

(defun dg--post-command-hook ()
  "Hook to auto-update context when moving to different node."
  (when (and discourse-graph-mode
             dg-context-auto-update
             (derived-mode-p 'org-mode)
             (get-buffer-window dg--context-buffer-name))
    (let ((id (dg--get-id-at-point)))
      (when (and id (not (equal id dg--current-node-id)))
        (run-with-idle-timer 0.2 nil (lambda () (dg--display-context id)))))))

;;; ============================================================
;;; Keybindings
;;; ============================================================

(defvar dg-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Creation
    (define-key map (kbd "C-c d c") #'dg-create-node)
    (define-key map (kbd "C-c d n q") #'dg-create-question)
    (define-key map (kbd "C-c d n c") #'dg-create-claim)
    (define-key map (kbd "C-c d n e") #'dg-create-evidence)
    (define-key map (kbd "C-c d n s") #'dg-create-source)
    ;; Relations
    (define-key map (kbd "C-c d r") #'dg-add-relation)
    (define-key map (kbd "C-c d R") #'dg-add-relation-with-context)
    ;; Navigation
    (define-key map (kbd "C-c d g") #'dg-goto-node)
    ;; Views
    (define-key map (kbd "C-c d x") #'dg-context-toggle)
    (define-key map (kbd "C-c d i") #'dg-node-index)
    (define-key map (kbd "C-c d q") #'dg-query-builder)
    (define-key map (kbd "C-c d s") #'dg-stats)
    ;; Export
    (define-key map (kbd "C-c d e d") #'dg-export-dot)
    (define-key map (kbd "C-c d e m") #'dg-export-markdown)
    ;; Cache
    (define-key map (kbd "C-c d !") #'dg-rebuild-cache)
    map)
  "Keymap for `discourse-graph-mode'.")

;;; ============================================================
;;; Minor Mode Definition
;;; ============================================================

;;;###autoload
(define-minor-mode discourse-graph-mode
  "Minor mode for discourse graph knowledge synthesis.

\\{dg-mode-map}"
  :global t
  :lighter " DG"
  :keymap dg-mode-map
  (if discourse-graph-mode
      (progn
        (add-hook 'after-save-hook #'dg--after-save-hook)
        (add-hook 'post-command-hook #'dg--post-command-hook)
        ;; Initialize database
        (dg--db)
        (message "Discourse Graph mode enabled"))
    (remove-hook 'after-save-hook #'dg--after-save-hook)
    (remove-hook 'post-command-hook #'dg--post-command-hook)
    (message "Discourse Graph mode disabled")))

(provide 'discourse-graph)
;;; discourse-graph.el ends here
