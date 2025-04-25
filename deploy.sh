#!/bin/bash
# Main deployment script for Apte Dictionary

# Check if we're in the right directory
if [ ! -f "serve.js" ]; then
    echo "Error: This script must be run from the project root directory"
    echo "Please run: ./deploy.sh"
    exit 1
fi

echo "=== Preparing deployment files ==="

# Create necessary directories
mkdir -p deploy/server deploy/public deploy/data/sharded

# Copy server files with modifications
cp serve.js deploy/server/

# Check if sanscript.js exists
if [ -f "sanscript.js" ]; then
    cp sanscript.js deploy/public/
else
    echo "Warning: sanscript.js not found in project root"
fi

# Copy term-viewer-simple.html
if [ -f "term-viewer-simple.html" ]; then
    cp term-viewer-simple.html deploy/public/
else
    echo "Error: term-viewer-simple.html not found in project root"
    exit 1
fi

echo "=== Deployment files prepared ==="
echo ""
echo "To deploy to Fly.io:"
echo "1. cd deploy"
echo "2. Run: fly launch (first time only)"
echo "3. Run: fly volumes create apte_data --size 3 (first time only)"
echo "4. Run: fly deploy"
echo ""
echo "To prepare and upload data:"
echo "1. cd deploy"
echo "2. Run: ./prepare-data.sh"
echo "3. Follow the instructions to upload data to your Fly.io app"
