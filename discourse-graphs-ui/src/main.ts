import './styles/main.css';
import ForceGraph, { ForceGraphInstance, NodeObject, LinkObject } from 'force-graph';
import * as d3 from 'd3';
import { WebSocketClient } from './websocket/WebSocketClient';
import { ColorManager } from './graph/ColorManager';
import { FilterManager } from './graph/FilterManager';
import { Node, Link, GraphData, ChainNode } from './types';
import { PHYSICS, NODE, TIMING, LAYOUT, THEME } from './constants';

class DiscourseGraphsUI {
  private ws: WebSocketClient;
  private graph!: ForceGraphInstance;
  private colorManager: ColorManager;
  private filterManager: FilterManager;

  private allNodes: Node[] = [];
  private allLinks: Link[] = [];
  private highlightNodes = new Set<Node>();
  private highlightLinks = new Set<Link>();
  private primaryHighlightNode: Node | null = null;
  private hoveredNode: Node | null = null;
  private currentSelectedNode: Node | null = null;
  private isSidebarCollapsed = false;
  private lastNodeClickTime: number = 0;
  private resizeTimeout: number | null = null;
  private currentChargeStrength: number = PHYSICS.CHARGE_STRENGTH;
  private currentLinkDistance: number = PHYSICS.LINK_DISTANCE;

  constructor() {
    this.colorManager = new ColorManager();
    this.filterManager = new FilterManager();
    this.ws = new WebSocketClient('ws://localhost:35904');
    
    this.initTheme();
    this.initGraph();
    this.setupWebSocket();
    this.setupEventListeners();
    this.setupKeyboardShortcuts();
  }

  private initGraph(): void {
    const container = document.getElementById('graph')!;
    const bgColor = this.getThemeBackgroundColor();

    this.graph = ForceGraph()(container)
      .backgroundColor(bgColor)
      .nodeRelSize(NODE.RELATIVE_SIZE)
      .nodeCanvasObject((node: NodeObject, ctx: CanvasRenderingContext2D, globalScale: number) =>
        this.renderNode(node as Node, ctx, globalScale))
      .onRenderFramePost((ctx: CanvasRenderingContext2D, globalScale: number) =>
        this.renderPrimaryNodeLabel(ctx, globalScale))
      .linkCanvasObject((link: LinkObject, ctx: CanvasRenderingContext2D, globalScale: number) =>
        this.renderLink(link as Link, ctx, globalScale))
      .linkDirectionalParticles((link: LinkObject) =>
        this.highlightLinks.has(link as Link) ? 4 : 2)
      .linkDirectionalParticleSpeed(0.004)
      .linkDirectionalParticleWidth((link: LinkObject) =>
        this.highlightLinks.has(link as Link) ? 3 : 2)
      .linkDirectionalParticleColor((link: LinkObject) =>
        this.colorManager.getLinkColor((link as Link).type))
      .onNodeHover((node: NodeObject | null) =>
        this.handleNodeHover(node as Node | null))
      .onNodeClick((node: NodeObject) =>
        this.handleNodeClick(node as Node))
      .onNodeRightClick((node: NodeObject) =>
        this.handleNodeRightClick(node as Node))
      .onNodeDragEnd((node: NodeObject) => {
        const n = node as Node;
        n.fx = n.x;
        n.fy = n.y;
        setTimeout(() => {
          n.fx = null;
          n.fy = null;
        }, TIMING.ZOOM_DURATION);
      })
      .d3VelocityDecay(PHYSICS.VELOCITY_DECAY)
      .cooldownTicks(PHYSICS.COOLDOWN_TICKS)
      .warmupTicks(PHYSICS.WARMUP_TICKS);
    
    // Configure forces for better behavior
    this.configureForces();
    
    // Setup background click to clear selection
    this.setupBackgroundClick();
  }

  private configureForces(): void {
    // Charge force (repulsion) - use current slider value
    this.graph.d3Force('charge', d3.forceManyBody()
      .strength(-this.currentChargeStrength)
      .distanceMax(PHYSICS.CHARGE_DISTANCE_MAX)
    );

    // Link force - use current slider value
    this.graph.d3Force('link', d3.forceLink()
      .distance(this.currentLinkDistance)
      .strength(0.6)
    );

    // Center force
    this.graph.d3Force('center', d3.forceCenter(0, 0)
      .strength(PHYSICS.RADIAL_STRENGTH)
    );

    // Radial force - keep isolated nodes from flying away
    this.graph.d3Force('radial', d3.forceRadial(
      (node: d3.SimulationNodeDatum) => {
        const linkCount = this.allLinks.filter(link =>
          link.source === node || link.target === node
        ).length;
        
        // Isolated nodes: constrain within radius 200
        // Connected nodes: no radial constraint (radius 0 = no force)
        return linkCount === 0 ? 200 : 0;
      }
    ).strength((node: any) => {
      // Count the node's connections
      const linkCount = this.allLinks.filter(link => 
        link.source === node || link.target === node
      ).length;
      
      // Isolated nodes: strong pull to radius (0.5)
      // Connected nodes: no radial force
      return linkCount === 0 ? 0.5 : 0;
    }));
    
    // Collision force - prevent node overlap
    this.graph.d3Force('collision', d3.forceCollide()
      .radius((node: any) => {
        const nodeR = Math.sqrt(Math.max(0, node.val || 1)) * 3;
        return nodeR + 10;  // Add padding
      })
      .strength(0.7)  // Strong enough to prevent overlap
    );
  }

  private renderNode(node: Node, ctx: CanvasRenderingContext2D, globalScale: number): void {
    const fontSize = 12 / globalScale;
    ctx.font = `400 ${fontSize}px Inter, sans-serif`;  // Normal weight for non-selected nodes
    
    // Base node radius from value (importance)
    const baseNodeR = Math.sqrt(Math.max(0, node.val || 1)) * 3;
    
    // Ensure minimum pixel size when zoomed out
    // This prevents nodes from becoming invisible when viewing the full graph
    const minPixelRadius = 3.5;  // Minimum 3.5 pixels on screen
    const minWorldRadius = minPixelRadius / globalScale;  // Convert to world coordinates
    const nodeR = Math.max(minWorldRadius, baseNodeR);
    
    const isPrimary = node === this.primaryHighlightNode;
    const isSecondary = this.highlightNodes.has(node) && !isPrimary;
    const isLight = this.isLightTheme();

    let nodeColor: string;
    let textColor: string;
    
    if (isPrimary) {
      nodeColor = this.colorManager.getNodeColor(node.type);
      textColor = '#ffffff';
      
      // Glow effect
      ctx.beginPath();
      ctx.arc(node.x!, node.y!, nodeR * 2.5, 0, 2 * Math.PI);
      const gradient = ctx.createRadialGradient(node.x!, node.y!, nodeR, node.x!, node.y!, nodeR * 2.5);
      gradient.addColorStop(0, nodeColor + '80');
      gradient.addColorStop(1, nodeColor + '00');
      ctx.fillStyle = gradient;
      ctx.fill();
    } else if (isSecondary) {
      nodeColor = this.colorManager.getNodeColor(node.type);
      textColor = isLight ? '#1f2937' : '#e4e7eb';

      // Secondary glow
      ctx.beginPath();
      ctx.arc(node.x!, node.y!, nodeR * 1.4, 0, 2 * Math.PI);
      const gradient = ctx.createRadialGradient(node.x!, node.y!, nodeR, node.x!, node.y!, nodeR * 1.4);
      gradient.addColorStop(0, nodeColor + '40');
      gradient.addColorStop(1, nodeColor + '00');
      ctx.fillStyle = gradient;
      ctx.fill();
    } else {
      nodeColor = this.colorManager.getNodeColorDim(node.type, isLight);
      textColor = isLight ? '#6b7280' : '#6b7280';
    }

    // Draw node
    ctx.beginPath();
    ctx.arc(node.x!, node.y!, nodeR, 0, 2 * Math.PI);
    ctx.fillStyle = nodeColor;
    ctx.fill();

    // Primary node border
    if (isPrimary) {
      ctx.strokeStyle = isLight ? '#1f2937' : '#ffffff';
      ctx.lineWidth = 3 / globalScale;
      ctx.stroke();
    }
    
    // Dynamic label display (org-roam-ui style)
    // Labels appear based on node importance and zoom level
    const isHovered = node === this.hoveredNode;
    
    // Calculate whether to show label
    let showLabel = false;
    
    if (isPrimary) {
      // Primary (clicked) node: label rendered separately in renderPrimaryNodeLabel
      showLabel = false;
    } else if (isHovered) {
      // Hovered node: always show label
      showLabel = true;
    } else if (isSecondary) {
      // Secondary (connected to primary): show if zoom is reasonable
      showLabel = globalScale >= 0.6;
    } else {
      // Other nodes: dynamic based on importance and zoom
      // Similar to org-roam-ui: importance vs zoom threshold
      const importance = node.val || 1;  // Node size reflects importance (in-degree)
      const threshold = 1 / (globalScale * globalScale);  // Zoom threshold
      
      // Show label if node is important enough for current zoom level
      // At zoom 1.0: threshold = 1, show if val > 1 (most nodes)
      // At zoom 0.5: threshold = 4, show if val > 4 (only important nodes)
      // At zoom 0.3: threshold = 11, show if val > 11 (very few nodes)
      showLabel = importance > threshold;
    }
    
    if (showLabel && !isPrimary) {
      const label = node.title;
      const textY = node.y! + nodeR + fontSize + 4;
      
      // Theme-aware shadow
      if (isLight) {
        ctx.shadowColor = 'rgba(255, 255, 255, 0.9)';
        ctx.shadowBlur = 8;
        ctx.shadowOffsetX = 0;
        ctx.shadowOffsetY = 0;
      } else {
        ctx.shadowColor = 'rgba(0, 0, 0, 0.9)';
        ctx.shadowBlur = 6;
        ctx.shadowOffsetX = 0;
        ctx.shadowOffsetY = 2;
      }
      
      // Draw text
      ctx.textAlign = 'center';
      ctx.textBaseline = 'middle';
      ctx.fillStyle = textColor;
      ctx.fillText(label, node.x!, textY);
      
      // Reset shadow
      ctx.shadowColor = 'transparent';
      ctx.shadowBlur = 0;
      ctx.shadowOffsetX = 0;
      ctx.shadowOffsetY = 0;
    }
  }

  private renderPrimaryNodeLabel(ctx: CanvasRenderingContext2D, globalScale: number): void {
    if (!this.primaryHighlightNode) return;
    
    const node = this.primaryHighlightNode;
    const fontSize = 13 / globalScale;
    
    // Use same sizing logic as renderNode for consistency
    const baseNodeR = Math.sqrt(Math.max(0, node.val || 1)) * 3;
    const minPixelRadius = 3.5;
    const minWorldRadius = minPixelRadius / globalScale;
    const nodeR = Math.max(minWorldRadius, baseNodeR);
    const isLight = this.isLightTheme();

    ctx.font = `700 ${fontSize}px Inter, sans-serif`;
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';

    // Theme-aware text color and shadow
    if (isLight) {
      ctx.fillStyle = '#1f2937';
      ctx.shadowColor = 'rgba(255, 255, 255, 0.9)';
      ctx.shadowBlur = 8;
    } else {
      ctx.fillStyle = '#ffffff';
      ctx.shadowColor = 'rgba(0, 0, 0, 0.8)';
      ctx.shadowBlur = 4;
    }
    
    ctx.shadowOffsetX = 0;
    ctx.shadowOffsetY = 1;
    
    ctx.fillText(node.title, node.x!, node.y! + nodeR + fontSize + 2);
    
    ctx.shadowColor = 'transparent';
    ctx.shadowBlur = 0;
  }

  private renderLink(link: Link, ctx: CanvasRenderingContext2D, globalScale: number): void {
    const start = link.source as Node;
    const end = link.target as Node;
    
    if (!start.x || !start.y || !end.x || !end.y) return;
    
    const isHighlight = this.highlightLinks.has(link);
    const color = this.colorManager.getLinkColor(link.type);
    
    // Determine line style based on style property from Emacs dg-relation-types
    const isDashed = link.style === 'dashed';
    
    // Dynamic line width and opacity based on zoom level
    // Minimum line width to keep links visible when zoomed out
    const baseWidth = isHighlight ? 1.2 : 0.8;
    const minLineWidth = 0.6;  // Minimum pixel width
    const lineWidth = Math.max(minLineWidth, baseWidth);
    
    // Dynamic opacity based on zoom level
    // When zoomed out, fade links to reduce visual clutter
    let opacityValue: number;
    if (isHighlight) {
      // Highlighted links: always visible
      opacityValue = 0.73;  // 73% opacity (same as before)
    } else if (globalScale < NODE.LABEL_VISIBILITY_THRESHOLD) {
      // Extremely zoomed out: very faded
      opacityValue = 0.25;
    } else if (globalScale < 0.8) {
      // Zoomed out: faded
      opacityValue = 0.4;  // 40% opacity
    } else {
      // Normal or zoomed in: standard opacity
      opacityValue = 0.31;  // 31% opacity (same as before)
    }
    
    // Convert to hex opacity
    const opacity = Math.round(opacityValue * 255).toString(16).padStart(2, '0').toUpperCase();
    
    // Set dashed/solid style based on style property
    if (isDashed) {
      ctx.setLineDash([3, 3]); // dashed
    } else {
      ctx.setLineDash([]); // solid
    }
    
    // Draw line
    ctx.strokeStyle = color + opacity;
    ctx.lineWidth = lineWidth;
    ctx.beginPath();
    ctx.moveTo(start.x, start.y);
    ctx.lineTo(end.x, end.y);
    ctx.stroke();
    
    // Reset line dash
    ctx.setLineDash([]);
    
    // Calculate arrow position using target node's actual size (same as renderNode)
    const dx = end.x - start.x;
    const dy = end.y - start.y;
    const angle = Math.atan2(dy, dx);
    
    // Use same sizing logic as renderNode for consistency
    const baseNodeR = Math.sqrt(Math.max(0, end.val || 1)) * 3;
    const minPixelRadius = 3.5;
    const minWorldRadius = minPixelRadius / globalScale;
    const nodeR = Math.max(minWorldRadius, baseNodeR);
    
    const arrowX = end.x - Math.cos(angle) * nodeR;
    const arrowY = end.y - Math.sin(angle) * nodeR;
    
    // Draw arrow: small and refined (particle animation exists, arrow can be small)
    const arrowLength = isHighlight ? 4 : 3.5;
    const arrowAngle = Math.PI / 6; // narrower angle, more refined
    
    ctx.fillStyle = color + opacity;
    ctx.beginPath();
    ctx.moveTo(arrowX, arrowY);
    ctx.lineTo(
      arrowX - arrowLength * Math.cos(angle - arrowAngle),
      arrowY - arrowLength * Math.sin(angle - arrowAngle)
    );
    ctx.lineTo(
      arrowX - arrowLength * Math.cos(angle + arrowAngle),
      arrowY - arrowLength * Math.sin(angle + arrowAngle)
    );
    ctx.closePath();
    ctx.fill();
  }

  private handleNodeHover(node: Node | null): void {
    this.hoveredNode = node;  // Track hovered node
    this.highlightNodes.clear();
    this.highlightLinks.clear();
    
    if (node) {
      this.highlightNodes.add(node);
      
      this.allLinks.forEach(link => {
        if (link.source === node || link.target === node) {
          this.highlightLinks.add(link);
          this.highlightNodes.add(link.source as Node);
          this.highlightNodes.add(link.target as Node);
        }
      });
    } else {
      // Mouse left all nodes - no action needed for right panel
      // Right panel stays open until user clicks close or background
    }
    
    if (this.primaryHighlightNode) {
      this.highlightNodes.add(this.primaryHighlightNode);
    }
  }

  private handleNodeClick(node: Node): void {
    this.lastNodeClickTime = Date.now();
    this.currentSelectedNode = node;
    this.primaryHighlightNode = node;
    
    // Node is now highlighted, user can press 'E' to export or right-click to open in Emacs
  }

  private handleNodeRightClick(node: Node): void {
    this.openNode(node);
  }

  private handleBackgroundClick(): void {
    // Clear focus when clicking on empty space
    this.primaryHighlightNode = null;
    this.currentSelectedNode = null;
    this.highlightNodes.clear();
    this.highlightLinks.clear();
  }

  private setupBackgroundClick(): void {
    // Listen for clicks on the canvas to detect background clicks
    const canvas = document.querySelector('#graph canvas');
    if (canvas) {
      canvas.addEventListener('click', (event) => {
        // Check if click is on empty space (not handled by node click)
        // The graph library handles node clicks, so this only fires for background
        const target = event.target as HTMLElement;
        if (target.tagName === 'CANVAS') {
          // Small delay to let node click handler execute first
          setTimeout(() => {
            // If no node was clicked (currentSelectedNode wasn't just set)
            const clickTime = Date.now();
            if (!this.lastNodeClickTime || clickTime - this.lastNodeClickTime > 50) {
              this.handleBackgroundClick();
            }
          }, 10);
        }
      });
    }
  }

  private openNode(node: Node): void {
    if (!this.ws.isConnected()) {
      this.showToast('Not connected to Emacs');
      return;
    }
    
    this.ws.send({
      type: 'open',
      data: { id: node.id }
    });
    
    this.showToast('Opening in Emacs...');
  }

  private setupWebSocket(): void {
    this.ws.on('connected', () => {
      this.updateStatus(true);
    });

    this.ws.on('disconnected', () => {
      this.updateStatus(false);
    });

    this.ws.on('graphdata', (data: GraphData) => {
      this.updateGraphData(data);
    });

    this.ws.on('command', (data: { commandName: string; id?: string }) => {
      this.handleCommand(data);
    });

    this.ws.on('focus', (data: { id: string }) => {
      const node = this.allNodes.find(n => n.id === data.id);
      if (node) {
        this.primaryHighlightNode = node;
        this.currentSelectedNode = node;
        this.centerOnNode(node);
      }
    });

    this.ws.connect();
  }

  private updateGraphData(data: GraphData): void {
    // Calculate in-degree (how many links point TO each node)
    // In discourse graphs, a node with more incoming links is more "important"
    // - A claim supported by many evidences
    // - A source cited by many claims
    // - A question answered by many claims
    const inDegreeMap = new Map<string, number>();
    
    // Initialize all nodes with 0
    data.nodes.forEach(node => {
      inDegreeMap.set(node.id, 0);
    });
    
    // Count incoming links for each node
    data.links.forEach(link => {
      const targetId = typeof link.target === 'string' ? link.target : link.target.id;
      inDegreeMap.set(targetId, (inDegreeMap.get(targetId) || 0) + 1);
    });
    
    // Set node val based on in-degree
    // Use logarithmic scale to avoid huge size differences
    // Base size 1, increases with in-degree
    this.allNodes = data.nodes.map(node => ({
      ...node,
      val: 1 + Math.sqrt(inDegreeMap.get(node.id) || 0) * 0.5
    }));

    this.allLinks = data.links.map(link => ({
      source: link.source,
      target: link.target,
      type: link.type || 'default',
      style: link.style || 'solid' // Preserve style property, default to solid
    }));

    // Build color maps
    this.colorManager.buildColorMaps(this.allNodes, this.allLinks);
    
    // Update filter manager
    this.filterManager.updateTypes(this.allNodes);
    
    // Build filter UI
    this.buildFilterUI();
    
    // Apply filters and render
    this.applyFilters();
  }

  private buildFilterUI(): void {
    const container = document.getElementById('filterItems')!;
    container.innerHTML = '';
    
    const types = Array.from(this.filterManager.getAllTypes()).sort();
    types.forEach(type => {
      const color = this.colorManager.getNodeColor(type);
      
      const label = document.createElement('label');
      label.className = 'filter-item';
      
      const checkbox = document.createElement('input');
      checkbox.type = 'checkbox';
      checkbox.checked = true;
      
      const toggle = document.createElement('div');
      toggle.className = 'filter-toggle';
      
      const dot = document.createElement('div');
      dot.className = 'filter-dot';
      dot.style.background = color;
      
      const labelText = document.createElement('span');
      labelText.className = 'filter-label';
      labelText.textContent = type.charAt(0).toUpperCase() + type.slice(1);
      
      checkbox.addEventListener('change', () => {
        this.filterManager.toggleType(type, checkbox.checked);
        this.applyFilters();
      });
      
      label.appendChild(checkbox);
      label.appendChild(toggle);
      label.appendChild(dot);
      label.appendChild(labelText);
      container.appendChild(label);
    });
  }

  private applyFilters(): void {
    const enabledTypes = this.filterManager.getEnabledTypes();
    const filteredNodes = this.allNodes.filter(n => enabledTypes.has(n.type));
    const nodeIds = new Set(filteredNodes.map(n => n.id));
    const filteredLinks = this.allLinks.filter(l => 
      nodeIds.has((l.source as Node).id || (l.source as string)) &&
      nodeIds.has((l.target as Node).id || (l.target as string))
    );
    
    // Check if currently highlighted node is filtered out
    if (this.primaryHighlightNode && !nodeIds.has(this.primaryHighlightNode.id)) {
      // Clear highlights
      this.primaryHighlightNode = null;
      this.highlightNodes.clear();
      this.highlightLinks.clear();
    }
    
    // Preserve node positions from previous state
    const currentData = this.graph.graphData();
    const positionMap = new Map<string, { x: number; y: number; vx?: number; vy?: number }>();
    
    if (currentData && currentData.nodes) {
      currentData.nodes.forEach((node: any) => {
        if (node.x !== undefined && node.y !== undefined) {
          positionMap.set(node.id, { 
            x: node.x, 
            y: node.y,
            vx: node.vx || 0,
            vy: node.vy || 0
          });
        }
      });
    }
    
    // Restore positions to filtered nodes and dampen velocities
    filteredNodes.forEach(node => {
      const savedPos = positionMap.get(node.id);
      if (savedPos) {
        node.x = savedPos.x;
        node.y = savedPos.y;
        // Dampen velocities to prevent sudden movements
        node.vx = (savedPos.vx || 0) * 0.3;
        node.vy = (savedPos.vy || 0) * 0.3;
        // Don't fix position
        node.fx = null;
        node.fy = null;
      }
    });
    
    // Update graph data
    this.graph.graphData({ nodes: filteredNodes, links: filteredLinks });
    
    // Update stats
    document.getElementById('nodeCount')!.textContent = filteredNodes.length.toString();
    document.getElementById('linkCount')!.textContent = filteredLinks.length.toString();
    
    // Gently reheat simulation with very low alpha for smooth transition
    const d3Sim = this.graph.d3Force('simulation');
    if (d3Sim) {
      d3Sim.alpha(0.2).alphaTarget(0).restart();
    }
    
    // Reconfigure forces to handle new graph structure
    this.configureForces();
    
    // Auto-fit view with smooth animation (delayed for better effect)
    if (filteredNodes.length > 0) {
      setTimeout(() => {
        this.graph.zoomToFit(TIMING.AUTO_FIT_DURATION, LAYOUT.FIT_PADDING);
      }, 300);
    }
  }

  private setupEventListeners(): void {
    // Search
    const searchBox = document.getElementById('searchBox') as HTMLInputElement;
    searchBox.addEventListener('input', (e) => {
      this.handleSearch((e.target as HTMLInputElement).value);
    });

    // Physics controls
    const chargeSlider = document.getElementById('chargeSlider') as HTMLInputElement;
    chargeSlider.addEventListener('input', (e) => {
      const value = Number((e.target as HTMLInputElement).value);
      this.currentChargeStrength = value;
      document.getElementById('chargeValue')!.textContent = String(value);
      this.graph.d3Force('charge', d3.forceManyBody()
        .strength(-value)
        .distanceMax(PHYSICS.CHARGE_DISTANCE_MAX)
      );
      this.graph.d3ReheatSimulation();
    });

    const distanceSlider = document.getElementById('distanceSlider') as HTMLInputElement;
    distanceSlider.addEventListener('input', (e) => {
      const value = Number((e.target as HTMLInputElement).value);
      this.currentLinkDistance = value;
      document.getElementById('distanceValue')!.textContent = String(value);
      this.graph.d3Force('link').distance(value);
      this.graph.d3ReheatSimulation();
    });

    // Sidebar toggle
    const sidebarToggle = document.getElementById('sidebarToggle')!;
    sidebarToggle.addEventListener('click', () => {
      this.toggleSidebar();
    });

    // Theme toggle
    const themeToggle = document.getElementById('themeToggle')!;
    themeToggle.addEventListener('click', () => {
      this.toggleTheme();
    });

    // Window resize with debounce
    window.addEventListener('resize', () => {
      if (this.resizeTimeout !== null) {
        clearTimeout(this.resizeTimeout);
      }
      this.resizeTimeout = window.setTimeout(() => {
        const container = document.getElementById('graph')!;
        const oldWidth = this.graph.width();
        const oldHeight = this.graph.height();
        const currentScale = this.graph.zoom();
        const currentCenter = this.graph.centerAt();

        const newWidth = container.clientWidth;
        const newHeight = container.clientHeight;

        this.graph.width(newWidth).height(newHeight);

        const widthDiff = newWidth - oldWidth;
        const heightDiff = newHeight - oldHeight;
        const offsetX = widthDiff / 2;
        const offsetY = heightDiff / 2;

        if (currentCenter.x !== undefined && currentCenter.y !== undefined) {
          this.graph.centerAt(
            currentCenter.x - offsetX / currentScale,
            currentCenter.y - offsetY / currentScale,
            300
          );
        }
      }, 100);
    });
  }

  private setupKeyboardShortcuts(): void {
    document.addEventListener('keydown', (e) => {
      if (e.key === 'r' || e.key === 'R') {
        if (this.ws.isConnected()) {
          this.ws.send({ type: 'requestGraphData' });
        }
      } else if (e.key === 'Enter' && this.currentSelectedNode) {
        this.openNode(this.currentSelectedNode);
      } else if (e.key === '[' || e.key === ']') {
        this.toggleSidebar();
      } else if (e.key === '/' || (e.key === 'f' && e.ctrlKey)) {
        e.preventDefault();
        // Expand sidebar if collapsed
        if (this.isSidebarCollapsed) {
          this.toggleSidebar();
        }
        (document.getElementById('searchBox') as HTMLInputElement).focus();
      } else if (e.key === '?' || (e.key === 'h' && !e.ctrlKey && !e.metaKey)) {
        this.showHelp();
      } else if ((e.key === 'e' || e.key === 'E') && this.currentSelectedNode) {
        // Export argument chain as Markdown
        e.preventDefault();
        this.exportArgumentChain(this.currentSelectedNode);
      }
    });
  }

  private showHelp(): void {
    const helpText = `
🎮 Controls

Mouse:
• Click node = Show details in right panel
• Right-click node = Open in Emacs
• Drag node = Adjust node position
• Drag background = Pan the graph ⭐
• Scroll wheel = Zoom

Keyboard shortcuts:
• / or Ctrl+F = Search nodes
• R = Refresh data
• [ or ] = Toggle left sidebar
• Enter = Open selected node
• E = Export argument chain (with full content)
• ? or H = Show this help

Right Panel:
• 📋 Copy = Copy node content
• 📂 Open = Open in Emacs
• 📋 Copy Chain = Export argument chain
• Click context nodes = Navigate

Tip:
Want to move the entire graph?
→ Just drag the background to pan the canvas!
    `.trim();
    
    this.showToast(helpText, 10000);
  }

  private exportArgumentChain(node: Node): void {
    // Build the argument chain structure
    const chain = this.buildArgumentChain(node);
    
    // Generate Markdown
    const text = this.formatAsMarkdown(node, chain);

    // Copy to clipboard
    this.copyToClipboard(text);
    
    this.showToast('Argument chain exported!', TIMING.TOAST_DURATION);
  }

  private buildArgumentChain(rootNode: Node): Map<string, ChainNode> {
    const chainNodes = new Map<string, ChainNode>();
    const visited = new Set<string>();
    
    // Recursive function to build chain
    const addNodeAndRelations = (nodeId: string, depth: number = 0) => {
      // Prevent infinite recursion
      if (depth > 10 || visited.has(nodeId)) return;
      visited.add(nodeId);
      
      const node = this.allNodes.find(n => n.id === nodeId);
      if (!node) return;
      
      // Add node to chain if not already there
      if (!chainNodes.has(nodeId)) {
        chainNodes.set(nodeId, {
          node: node,
          supporters: [],
          opposers: [],
          informers: [],
          answerers: []
        });
      }
      
      const chainNode = chainNodes.get(nodeId)!;
      
      // Find all links where this node is the target
      const incomingLinks = this.allLinks.filter(link => {
        const target = typeof link.target === 'string' ? link.target : (link.target as Node).id;
        return target === nodeId;
      });
      
      // Process each incoming link
      incomingLinks.forEach(link => {
        const sourceId = typeof link.source === 'string' ? link.source : (link.source as Node).id;
        const sourceNode = this.allNodes.find(n => n.id === sourceId);
        
        if (!sourceNode) return;
        
        // Add source node to appropriate category
        if (link.type === 'supports' && !chainNode.supporters.find(n => n.id === sourceId)) {
          chainNode.supporters.push(sourceNode);
          addNodeAndRelations(sourceId, depth + 1);
        } else if (link.type === 'opposes' && !chainNode.opposers.find(n => n.id === sourceId)) {
          chainNode.opposers.push(sourceNode);
          addNodeAndRelations(sourceId, depth + 1);
        } else if (link.type === 'informs' && !chainNode.informers.find(n => n.id === sourceId)) {
          chainNode.informers.push(sourceNode);
          addNodeAndRelations(sourceId, depth + 1);
        } else if (link.type === 'answers' && !chainNode.answerers.find(n => n.id === sourceId)) {
          chainNode.answerers.push(sourceNode);
          addNodeAndRelations(sourceId, depth + 1);
        }
      });
    };
    
    // Start building from root
    addNodeAndRelations(rootNode.id);
    
    return chainNodes;
  }

  private cleanOrgContent(content: string): string {
    if (!content) return '';
    
    let cleaned = content;
    
    // Remove first heading line
    cleaned = cleaned.replace(/^\*+ .*$/m, '');
    
    // Remove PROPERTIES drawers
    cleaned = cleaned.replace(/:PROPERTIES:[\s\S]*?:END:/g, '');
    
    // Remove LOGBOOK drawers
    cleaned = cleaned.replace(/:LOGBOOK:[\s\S]*?:END:/g, '');
    
    // Convert org sub-headings to markdown (from deepest to shallowest)
    cleaned = cleaned.replace(/^\*\*\*\*\* (.*)$/gm, '##### $1');
    cleaned = cleaned.replace(/^\*\*\*\* (.*)$/gm, '#### $1');
    cleaned = cleaned.replace(/^\*\*\* (.*)$/gm, '### $1');
    cleaned = cleaned.replace(/^\*\* (.*)$/gm, '## $1');
    cleaned = cleaned.replace(/^\* (.*)$/gm, '# $1');
    
    // Clean up excessive newlines
    cleaned = cleaned.replace(/\n\n\n+/g, '\n\n');
    
    return cleaned.trim();
  }

  private formatAsMarkdown(rootNode: Node, chain: Map<string, ChainNode>): string {
    const lines: string[] = [];
    const rootChainNode = chain.get(rootNode.id);

    if (!rootChainNode) return '';

    // Title
    lines.push('# Argument Chain\n');
    
    // Main node with type short (QUE-Title format)
    const typeShort = rootNode.typeShort || rootNode.type.toUpperCase();
    lines.push(`## ${typeShort}-${rootNode.title}\n`);
    
    // Content (cleaned)
    if (rootNode.content) {
      const cleaned = this.cleanOrgContent(rootNode.content);
      if (cleaned) {
        lines.push('\n' + cleaned + '\n');
      }
    }

    // Supporting evidence
    if (rootChainNode.supporters.length > 0) {
      lines.push('\n**Supported By:**\n');
      rootChainNode.supporters.forEach(node => {
        const short = node.typeShort || node.type.toUpperCase();
        lines.push(`- **${short}-${node.title}**`);
        if (node.content) {
          const cleaned = this.cleanOrgContent(node.content);
          if (cleaned) {
            const indented = cleaned.split('\n').map(l => '  ' + l).join('\n');
            lines.push('  ');
            lines.push(indented);
            lines.push('');
          }
        }
      });
      lines.push('');
    }

    // Counter-arguments
    if (rootChainNode.opposers.length > 0) {
      lines.push('**Opposed By:**\n');
      rootChainNode.opposers.forEach(node => {
        const short = node.typeShort || node.type.toUpperCase();
        lines.push(`- **${short}-${node.title}**`);
        if (node.content) {
          const cleaned = this.cleanOrgContent(node.content);
          if (cleaned) {
            const indented = cleaned.split('\n').map(l => '  ' + l).join('\n');
            lines.push('  ');
            lines.push(indented);
            lines.push('');
          }
        }
      });
      lines.push('');
    }

    // Context
    if (rootChainNode.informers.length > 0) {
      lines.push('**Informed By:**\n');
      rootChainNode.informers.forEach(node => {
        const short = node.typeShort || node.type.toUpperCase();
        lines.push(`- ${short}-${node.title}`);
      });
      lines.push('');
    }

    // Answers
    if (rootChainNode.answerers.length > 0) {
      lines.push('**Answered By:**\n');
      rootChainNode.answerers.forEach(node => {
        const short = node.typeShort || node.type.toUpperCase();
        lines.push(`- ${short}-${node.title}`);
      });
      lines.push('');
    }

    // Metadata
    lines.push('---');
    lines.push('*Exported from Discourse Graph*');
    lines.push(`*Total nodes: ${chain.size}*`);

    return lines.join('\n');
  }

  private async copyToClipboard(text: string): Promise<void> {
    try {
      await navigator.clipboard.writeText(text);
    } catch (err) {
      // Fallback for older browsers
      const textarea = document.createElement('textarea');
      textarea.value = text;
      textarea.style.position = 'fixed';
      textarea.style.opacity = '0';
      document.body.appendChild(textarea);
      textarea.select();
      document.execCommand('copy');
      document.body.removeChild(textarea);
    }
  }

  private handleSearch(query: string): void {
    if (!query.trim()) {
      this.primaryHighlightNode = null;
      return;
    }
    
    const matches = this.allNodes.filter(n => 
      n.title.toLowerCase().includes(query.toLowerCase())
    );
    
    if (matches.length > 0) {
      this.primaryHighlightNode = matches[0];
      this.currentSelectedNode = matches[0];
      this.centerOnNode(matches[0]);
      this.showToast(`Found: ${matches[0].title.substring(0, 30)}...`);
    } else {
      this.showToast('No matches found');
    }
  }

  private centerOnNode(node: Node, zoom: number = LAYOUT.DEFAULT_ZOOM, duration: number = TIMING.CENTER_DURATION): void {
    const x = node.x ?? 0;
    const y = node.y ?? 0;
    this.graph.centerAt(x, y, duration);
    this.graph.zoom(zoom, duration);
  }

  private toggleSidebar(): void {
    const sidebar = document.getElementById('sidebar')!;
    const graphEl = document.getElementById('graph')!;
    const wasCollapsed = this.isSidebarCollapsed;
    
    this.isSidebarCollapsed = !this.isSidebarCollapsed;
    
    if (this.isSidebarCollapsed) {
      sidebar.classList.add('collapsed');
    } else {
      sidebar.classList.remove('collapsed');
    }
    
    // Wait for CSS transition, then shift graph
    setTimeout(() => {
      // Resize graph to new dimensions
      this.graph
        .width(graphEl.clientWidth)
        .height(graphEl.clientHeight);
      
      // Get current camera position
      const currentCenter = this.graph.centerAt();
      const currentScale = this.graph.zoom();
      
      // Calculate shift amount in graph coordinates
      // Camera movement is OPPOSITE to visual content movement
      const shiftDirection = wasCollapsed ? -1 : 1;
      const shiftAmount = (LAYOUT.SIDEBAR_WIDTH / 2) / currentScale;

      // Smoothly shift the camera
      if (currentCenter.x !== undefined && currentCenter.y !== undefined) {
        this.graph.centerAt(
          currentCenter.x + (shiftDirection * shiftAmount),
          currentCenter.y,
          TIMING.RESIZE_TRANSITION + 200
        );
      }
    }, TIMING.RESIZE_TRANSITION + 20);
  }

  private handleCommand(data: any): void {
    if (data.commandName === 'follow' && data.id) {
      const node = this.allNodes.find(n => n.id === data.id);
      if (node) {
        const indicator = document.getElementById('followIndicator')!;
        indicator.classList.add('active');
        setTimeout(() => {
          indicator.classList.remove('active');
        }, 2000);
        
        this.highlightNodes.clear();
        this.highlightLinks.clear();
        this.primaryHighlightNode = node;
        this.highlightNodes.add(node);
        this.currentSelectedNode = node;
        
        this.centerOnNode(node, 2.5, 1000);
        this.showToast('🎯 Following: ' + node.title.substring(0, 30) + '...');
      }
    }
  }

  private updateStatus(connected: boolean): void {
    const statusEl = document.getElementById('status')!;
    if (connected) {
      statusEl.className = 'status-badge connected';
      statusEl.innerHTML = '<span class="status-dot"></span> Connected';
    } else {
      statusEl.className = 'status-badge disconnected';
      statusEl.innerHTML = '<span class="status-dot"></span> Disconnected';
    }
  }

  private showToast(message: string, duration: number = 2000): void {
    const toast = document.getElementById('toast')!;
    toast.textContent = message;
    toast.style.whiteSpace = 'pre-line';  // Support multi-line messages
    toast.classList.add('visible');
    setTimeout(() => {
      toast.classList.remove('visible');
    }, duration);
  }

  private initTheme(): void {
    // Load theme from localStorage
    const savedTheme = localStorage.getItem('dg-theme');
    if (savedTheme === 'light') {
      document.body.classList.add('light-theme');
    }
  }

  /** Check if light theme is active */
  private isLightTheme(): boolean {
    return document.body.classList.contains('light-theme');
  }

  /** Get background color based on current theme */
  private getThemeBackgroundColor(): string {
    return this.isLightTheme() ? THEME.LIGHT.BACKGROUND : THEME.DARK.BACKGROUND;
  }

  private toggleTheme(): void {
    const isLight = document.body.classList.toggle('light-theme');
    localStorage.setItem('dg-theme', isLight ? 'light' : 'dark');
    this.graph.backgroundColor(this.getThemeBackgroundColor());
    this.showToast(isLight ? 'Switched to Light theme' : 'Switched to Dark theme');
  }

}

// Initialize app
new DiscourseGraphsUI();
console.log('🚀 Discourse Graphs UI started');
