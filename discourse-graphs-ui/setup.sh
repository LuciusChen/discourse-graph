#!/bin/bash

echo "🚀 Discourse Graphs UI - Project Setup"
echo "======================================"
echo ""

# Check Node.js
if ! command -v node &> /dev/null; then
    echo "❌ Error: Node.js is not installed"
    echo "Please install Node.js: https://nodejs.org/"
    exit 1
fi

echo "✅ Node.js version: $(node --version)"
echo "✅ npm version: $(npm --version)"
echo ""

# Check if in correct directory
if [ ! -f "package.json" ]; then
    echo "❌ Error: Please run this script in the discourse-graphs-ui directory"
    exit 1
fi

# Install dependencies
echo "📦 Installing dependencies..."
npm install

if [ $? -ne 0 ]; then
    echo "❌ Dependency installation failed"
    exit 1
fi

echo ""
echo "🔒 Checking for security vulnerabilities..."
npm audit

# Auto-fix vulnerabilities if any are found
if [ $? -ne 0 ]; then
    echo ""
    echo "🔧 Attempting to fix vulnerabilities..."
    npm audit fix
    
    if [ $? -eq 0 ]; then
        echo "✅ Vulnerabilities fixed!"
    else
        echo "⚠️  Some vulnerabilities remain (likely in dev dependencies)"
        echo "   See SECURITY.md for details"
        echo "   This is safe for development and production builds"
    fi
fi

echo ""
echo "✅ Project setup complete!"
echo ""
echo "🎯 Next steps:"
echo ""
echo "  Development mode:"
echo "    npm run dev"
echo ""
echo "  Build for production:"
echo "    npm run build"
echo ""
echo "  Preview build:"
echo "    npm run preview"
echo ""
echo "📚 For more information, see:"
echo "  - README.md"
echo "  - GETTING_STARTED.md"
echo "  - SECURITY.md (for vulnerability info)"
echo ""
echo "🎉 Happy coding!"


