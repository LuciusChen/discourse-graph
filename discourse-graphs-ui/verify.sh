#!/bin/bash

echo "🔍 验证 TypeScript 项目配置"
echo "================================"
echo ""

# 检查文件是否存在
echo "✅ 检查关键文件..."
files=(
    "package.json"
    "tsconfig.json"
    "vite.config.ts"
    "src/main.ts"
    "src/types/force-graph.d.ts"
)

for file in "${files[@]}"; do
    if [ -f "$file" ]; then
        echo "   ✅ $file"
    else
        echo "   ❌ $file - 缺失！"
        exit 1
    fi
done

echo ""
echo "✅ 所有关键文件存在"
echo ""

# 检查 node_modules
if [ ! -d "node_modules" ]; then
    echo "⚠️  node_modules 不存在"
    echo "   请运行: npm install"
    echo ""
fi

# 尝试 TypeScript 编译检查（如果有 node_modules）
if [ -d "node_modules" ]; then
    echo "🔍 检查 TypeScript 编译..."
    if npx tsc --noEmit 2>&1 | grep -q "error TS"; then
        echo "   ❌ TypeScript 编译有错误"
        npx tsc --noEmit
        exit 1
    else
        echo "   ✅ TypeScript 编译通过"
    fi
else
    echo "⏭️  跳过 TypeScript 检查（需要先安装依赖）"
fi

echo ""
echo "🎉 项目配置验证完成！"
echo ""
echo "下一步："
echo "  1. npm install    # 安装依赖"
echo "  2. npm run dev    # 启动开发服务器"
echo "  3. npm run build  # 构建生产版本"
echo ""
