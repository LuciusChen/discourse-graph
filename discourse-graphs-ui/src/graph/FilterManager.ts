import { Node } from '../types';

export class FilterManager {
  private enabledTypes = new Set<string>();
  private allTypes = new Set<string>();

  updateTypes(nodes: Node[]): void {
    this.allTypes.clear();
    nodes.forEach(node => {
      if (node.type) {
        this.allTypes.add(node.type);
      }
    });
    
    // Enable all types by default
    this.enabledTypes = new Set(this.allTypes);
  }

  getAllTypes(): Set<string> {
    return new Set(this.allTypes);
  }

  toggleType(type: string, enabled: boolean): void {
    if (enabled) {
      this.enabledTypes.add(type);
    } else {
      this.enabledTypes.delete(type);
    }
  }

  getEnabledTypes(): Set<string> {
    return new Set(this.enabledTypes);
  }
}
