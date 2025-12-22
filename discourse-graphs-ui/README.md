# Discourse Graphs UI

Interactive web visualization for discourse-graphs.el, built with Vite + TypeScript + force-graph.

## 🎯 For Users

**You don't need to build anything!** The pre-built UI is included in the `out/` directory.

Just use `M-x dg-ui-open` in Emacs and it will work automatically.

---

## 🛠️ For Developers

### Prerequisites

- Node.js 20+ (LTS recommended)
- npm 9+

### Quick Start

```bash
# Install dependencies
./setup.sh
# or manually:
npm install

# Development (with hot reload)
npm run dev

# Build for production
npm run build

# Preview production build
npm run preview
```

### Project Structure

```
src/
├── graph/
│   ├── ColorManager.ts      # Node/link colors
│   └── FilterManager.ts     # Type filtering
├── websocket/
│   └── WebSocketClient.ts   # Emacs communication
├── ui/
│   └── PanelManager.ts      # Sidebar management
├── types/
│   ├── index.ts             # Data types
│   └── force-graph.d.ts     # Force-graph types
├── styles/
│   └── main.css             # Styling
└── main.ts                  # Entry point
```

### Features

- **Real-time sync** — WebSocket connection to Emacs
- **Force-directed layout** — Automatic node positioning
- **Interactive controls** — Drag, zoom, search, filter
- **Type-based colors** — Auto-generated color scheme
- **Dark/Light theme** — Toggle with theme button

### How It Works

1. **Emacs starts servers**:
   - HTTP server (port 8080) serves `out/index.html`
   - WebSocket server (port 35904) for data sync

2. **UI connects**:
   - Loads in browser at `http://localhost:8080`
   - Connects to WebSocket for graph data
   - Displays interactive force-directed graph

3. **User interactions**:
   - Right-click node → Opens in Emacs
   - Search, filter, zoom → All work locally
   - Emacs changes → Auto-sync via WebSocket

### Building

The build produces a single-file bundle in `out/`:

```bash
npm run build
```

Output:
- `out/index.html` — Single HTML file with inlined CSS/JS
- `out/assets/` — Font and other assets

This is what gets included in the discourse-graphs package.

### Development Tips

**Hot Reload:**
```bash
npm run dev
```
Opens at `http://localhost:3000` with instant updates.

**Testing with Emacs:**
1. Build: `npm run build`
2. In Emacs: `M-x dg-ui-open`
3. Browser opens with your changes

**Force-graph API:**
See `src/types/force-graph.d.ts` for available methods.

### Security Note

You may see warnings during `npm install`:
- Node.js v23 experimental warnings → Safe, just Node.js issues
- npm audit vulnerabilities → Only affect dev server, not production build

See `SECURITY.md` for details.

### Adding Features

1. Create module in appropriate directory
2. Import in `main.ts`
3. Add to class if needed
4. Hot reload shows changes instantly

### Customization

**Colors:** Edit `src/styles/main.css` CSS variables

**Physics:** Adjust in `src/main.ts`:
```typescript
.d3VelocityDecay(0.3)
.cooldownTicks(100)
.warmupTicks(50)
```

**Layout:** Modify `src/graph/ColorManager.ts` for color schemes

---

## 📦 Distribution

When publishing discourse-graphs:

1. Build the UI: `npm run build`
2. Commit `out/` directory to git
3. Users get pre-built files automatically

No build step needed for end users!

---

## 🐛 Troubleshooting

**Build fails:**
```bash
rm -rf node_modules package-lock.json
npm install
npm run build
```

**TypeScript errors:**
Check that all types are defined in `src/types/`

**WebSocket connection fails:**
- Check Emacs is running `dg-ui-start-server`
- Verify port 35904 is available
- Check browser console (F12) for errors

---

## 📝 License

GPL-3.0 (same as discourse-graphs.el)
