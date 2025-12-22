import { defineConfig } from 'vite';

export default defineConfig({
  build: {
    outDir: 'out',
    rollupOptions: {
      output: {
        // 生成单个 HTML 文件（所有资源内联）
        inlineDynamicImports: true,
        manualChunks: undefined,
      }
    }
  },
  server: {
    port: 3000,
    open: true
  }
});
