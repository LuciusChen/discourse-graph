import { Node } from '../types';

export class FilterManager {
  private enabledTypes = new Set<string>();
  private allTypes = new Set<string>();
  private onChangeCallback: (() => void) | null = null;

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
    
    if (this.onChangeCallback) {
      this.onChangeCallback();
    }
  }

  isTypeEnabled(type: string): boolean {
    return this.enabledTypes.has(type);
  }

  onChange(callback: () => void): void {
    this.onChangeCallback = callback;
  }

  getEnabledTypes(): Set<string> {
    return new Set(this.enabledTypes);
  }
}
