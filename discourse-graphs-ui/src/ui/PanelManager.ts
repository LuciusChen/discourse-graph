import { Node } from '../types';

export class PanelManager {
  private panel: HTMLElement;
  private titleEl: HTMLElement;
  private metaEl: HTMLElement;
  private fileEl: HTMLElement;
  private openButton: HTMLElement;
  private hideTimeout: number | null = null;
  private onOpenCallback: ((node: Node) => void) | null = null;

  constructor() {
    this.panel = document.getElementById('nodePanel')!;
    this.titleEl = document.getElementById('panelTitle')!;
    this.metaEl = document.getElementById('panelMeta')!;
    this.fileEl = document.getElementById('panelFile')!;
    this.openButton = document.getElementById('openButton')!;

    this.setupEventListeners();
  }

  private setupEventListeners(): void {
    // Mouse enter panel - cancel hide
    this.panel.addEventListener('mouseenter', () => {
      if (this.hideTimeout) {
        clearTimeout(this.hideTimeout);
        this.hideTimeout = null;
      }
    });

    // Mouse leave panel - hide with fade out
    this.panel.addEventListener('mouseleave', () => {
      this.hide();
    });

    // Open button
    this.openButton.addEventListener('click', () => {
      if (this.onOpenCallback) {
        const node = (this.openButton as any)._currentNode;
        if (node) {
          this.onOpenCallback(node);
        }
      }
    });
  }

  show(node: Node): void {
    // Clear any pending hide
    if (this.hideTimeout) {
      clearTimeout(this.hideTimeout);
      this.hideTimeout = null;
    }

    // Update content
    this.titleEl.textContent = node.title;
    this.metaEl.innerHTML = `
      <div class="node-panel-tag">Type: ${node.type}</div>
      <div class="node-panel-tag">ID: ${node.id.substring(0, 8)}...</div>
    `;
    this.fileEl.textContent = node.file;

    // Store node reference
    (this.openButton as any)._currentNode = node;

    // Show with fade in
    this.panel.classList.add('visible');
  }

  hide(): void {
    this.panel.classList.remove('visible');
  }

  scheduleHide(delay: number = 1000): void {
    if (this.hideTimeout) {
      clearTimeout(this.hideTimeout);
    }
    this.hideTimeout = window.setTimeout(() => {
      this.hide();
    }, delay);
  }

  onOpen(callback: (node: Node) => void): void {
    this.onOpenCallback = callback;
  }
}
