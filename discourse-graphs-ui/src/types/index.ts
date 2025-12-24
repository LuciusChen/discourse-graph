export interface Node {
  id: string;
  title: string;
  type: string;
  typeShort?: string;  // Short abbreviation like "QUE", "CLM", etc.
  file: string;
  pos?: number;
  content?: string;  // Full org content including sub-headings
  val: number;
  x?: number;
  y?: number;
  vx?: number;  // velocity x
  vy?: number;  // velocity y
  fx?: number | null;
  fy?: number | null;
}

export interface Link {
  source: Node | string;
  target: Node | string;
  type: string;
  style?: string;  // 'solid' or 'dashed'
}

export interface GraphData {
  nodes: Node[];
  links: Link[];
}

export interface WebSocketMessage {
  type: 'graphdata' | 'theme' | 'command';
  data: any;
}

export interface CommandData {
  commandName: string;
  id?: string;
}

export interface ThemeData {
  colors?: Record<string, string>;
}

export interface ChainNode {
  node: Node;
  supporters: Node[];
  opposers: Node[];
  informers: Node[];
  answerers: Node[];
}
