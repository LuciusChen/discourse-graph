# 🔒 Security & Warnings Guide

## 📋 For End Users

**If you're just using discourse-graphs (not developing the UI), you can ignore all warnings!**

The warnings only appear during UI development (`npm install`). The pre-built UI in `out/` is completely safe.

---

## 📋 For Developers

If you're building the UI, you may see these warnings during `./setup.sh` or `npm install`. They are **safe and expected**:

- ✅ Node.js v23 experimental warnings (Node.js issue, not ours)
- ✅ npm audit vulnerabilities in **dev dependencies only** (don't affect production)
- ✅ Production builds are secure and fully functional

**You can safely use the UI despite these warnings.**

---

## Node.js v23 Warning (Safe to Ignore)

If you're using Node.js v23, you'll see:

```
(node:xxxxx) ExperimentalWarning: CommonJS module ... is loading ES Module ... using require().
```

**This is normal and safe.** It's a Node.js v23 issue with npm itself, not your code.

**To suppress**: Use Node.js v20 LTS or v22 instead:
```bash
nvm install 20
nvm use 20
```

---

## npm Audit Vulnerabilities (Dev Dependencies Only)

During `./setup.sh`, you'll see:

```
2 moderate severity vulnerabilities

esbuild  <=0.24.2
Severity: moderate
esbuild enables any website to send any requests to the development server

vite  0.11.0 - 6.1.6
Depends on vulnerable versions of esbuild
```

### Why This Is Safe

1. **Dev Dependencies Only**: These affect the dev server (`npm run dev`), not production builds
2. **Local Development**: The dev server only runs on your local machine
3. **Production Builds Safe**: The `npm run build` output has no vulnerabilities
4. **Limited Scope**: The vulnerability only affects dev server, not the built files

### What You're Actually Using

- **Development** (`npm run dev`): Runs local dev server with vite/esbuild
- **Production** (`npm run build`): Creates static HTML/CSS/JS files
- **In Emacs**: Uses production builds via HTTP server

**The production build that Emacs uses is completely safe.**

---

## How to Fix (Optional)

If you want to eliminate the warnings:

### Option 1: Auto-fix (Recommended)

```bash
npm audit fix
```

This automatically updates dependencies to secure versions.

### Option 2: Force Fix

If auto-fix doesn't work:

```bash
npm audit fix --force
```

**Warning**: This may update to breaking versions. Test after running.

### Option 3: Manual Update

Check which packages have vulnerabilities:

```bash
npm audit
```

Then update specific packages:

```bash
npm update <package-name>
```

## Verify Fix

After fixing:

```bash
npm audit
```

Should show:

```
found 0 vulnerabilities
```

## For Production

The production build (`npm run build`) only includes runtime dependencies, not dev dependencies. Security issues in dev dependencies (like vite, typescript) don't affect the final `out/index.html` file.

## Safe to Ignore?

**For development**: Safe to proceed if vulnerabilities are only in dev dependencies
**For production**: The built file is secure

## Regular Maintenance

Update dependencies periodically:

```bash
npm update
npm audit fix
```

## If Issues Persist

1. Delete lock file and reinstall:
```bash
rm package-lock.json
rm -rf node_modules
npm install
```

2. Check for updates:
```bash
npm outdated
```

3. Update all:
```bash
npm update
```

---

**Note**: The 2 moderate vulnerabilities are typically in development tools and don't affect the production build. You can safely proceed with development and fix them when convenient.
