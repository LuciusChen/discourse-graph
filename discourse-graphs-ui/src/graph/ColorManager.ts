import { Node, Link } from '../types';

export class ColorManager {
  private nodeColorMap = new Map<string, string>();
  private linkColorMap = new Map<string, string>();
  private nodeColorMapDimDark = new Map<string, string>();  // For dark theme
  private nodeColorMapDimLight = new Map<string, string>(); // For light theme
  private linkColorMapDimDark = new Map<string, string>();
  private linkColorMapDimLight = new Map<string, string>();

  private readonly PREDEFINED_NODE_COLORS: Record<string, string> = {
    // org-roam-ui inspired colors - softer, more pleasant
    question: '#7c3aed',   // Purple - for questions/exploration
    claim: '#0ea5e9',      // Sky blue - for claims/statements  
    evidence: '#f97316',   // Orange - for evidence/data
    source: '#ec4899'      // Pink - for sources/references
  };

  private readonly PREDEFINED_LINK_COLORS: Record<string, string> = {
    supports: '#22c55e',   // Green - supportive relationship
    opposes: '#ef4444',    // Red - opposing relationship
    answers: '#8b5cf6',    // Purple - answering relationship
    informs: '#06b6d4',    // Cyan - informational relationship
    'based-on': '#a855f7'  // Purple variant
  };

  buildColorMaps(nodes: Node[], links: Link[]): void {
    // Collect all types
    const nodeTypes = new Set(nodes.map(n => n.type));
    const linkTypes = new Set(links.map(l => l.type));

    // Build node color map
    const nodeTypesArray = Array.from(nodeTypes).sort();
    nodeTypesArray.forEach((type, index) => {
      if (this.PREDEFINED_NODE_COLORS[type]) {
        this.nodeColorMap.set(type, this.PREDEFINED_NODE_COLORS[type]);
      } else {
        const color = this.generateColor(index, nodeTypesArray.length);
        this.nodeColorMap.set(type, color);
      }
      // Dim versions for different themes
      this.nodeColorMapDimDark.set(type, '#4a5568');   // Dark theme: darker gray
      this.nodeColorMapDimLight.set(type, '#cbd5e1');  // Light theme: lighter gray
    });
    this.nodeColorMap.set('default', '#64748b');
    this.nodeColorMapDimDark.set('default', '#4a5568');
    this.nodeColorMapDimLight.set('default', '#cbd5e1');

    // Build link color map
    const linkTypesArray = Array.from(linkTypes).sort();
    linkTypesArray.forEach((type, index) => {
      if (this.PREDEFINED_LINK_COLORS[type]) {
        this.linkColorMap.set(type, this.PREDEFINED_LINK_COLORS[type]);
      } else {
        const color = this.generateColor(index, linkTypesArray.length);
        this.linkColorMap.set(type, color);
      }
      // Dim versions for different themes
      this.linkColorMapDimDark.set(type, '#374151');   // Dark theme: darker gray
      this.linkColorMapDimLight.set(type, '#94a3b8');  // Light theme: lighter gray
    });
    this.linkColorMap.set('default', '#475569');
    this.linkColorMapDimDark.set('default', '#374151');
    this.linkColorMapDimLight.set('default', '#94a3b8');
  }

  private generateColor(index: number, total: number): string {
    const hue = (index * 360 / total) % 360;
    return this.hslToHex(hue, 70, 60);
  }

  private hslToHex(h: number, s: number, l: number): string {
    s /= 100;
    l /= 100;
    const c = (1 - Math.abs(2 * l - 1)) * s;
    const x = c * (1 - Math.abs((h / 60) % 2 - 1));
    const m = l - c / 2;
    let r = 0, g = 0, b = 0;

    if (0 <= h && h < 60) { r = c; g = x; b = 0; }
    else if (60 <= h && h < 120) { r = x; g = c; b = 0; }
    else if (120 <= h && h < 180) { r = 0; g = c; b = x; }
    else if (180 <= h && h < 240) { r = 0; g = x; b = c; }
    else if (240 <= h && h < 300) { r = x; g = 0; b = c; }
    else if (300 <= h && h < 360) { r = c; g = 0; b = x; }

    r = Math.round((r + m) * 255);
    g = Math.round((g + m) * 255);
    b = Math.round((b + m) * 255);

    return '#' + [r, g, b]
      .map(x => x.toString(16).padStart(2, '0'))
      .join('');
  }

  getNodeColor(type: string): string {
    return this.nodeColorMap.get(type) || this.nodeColorMap.get('default')!;
  }

  getLinkColor(type: string): string {
    return this.linkColorMap.get(type) || this.linkColorMap.get('default')!;
  }

  getNodeColorDim(type: string, isLightTheme: boolean = false): string {
    const map = isLightTheme ? this.nodeColorMapDimLight : this.nodeColorMapDimDark;
    return map.get(type) || map.get('default')!;
  }

  getLinkColorDim(type: string, isLightTheme: boolean = false): string {
    const map = isLightTheme ? this.linkColorMapDimLight : this.linkColorMapDimDark;
    return map.get(type) || map.get('default')!;
  }

  getAllNodeColors(): Map<string, string> {
    return new Map(this.nodeColorMap);
  }
}
