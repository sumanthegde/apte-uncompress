#!/bin/bash
# Script to prepare for deploying new code WITH new data

echo "Cleaning up from previous deployments..."
./cleanup.sh

echo "Preparing deployment with new data..."

# Create necessary directories
mkdir -p server public data

# Copy server files
cp ../serve.js server/

# Copy static files
cp ../term-viewer-simple.html public/
cp ../sanscript.js public/

# Create the data.zip file with real data
echo "Creating data.zip with real dictionary data..."
# Save current directory
CURRENT_DIR=$(pwd)
cd ../apteDir.nosync/output
/usr/bin/zip -vr data.zip pagemarks.json table_new.txt sharded -X
# Return to original directory
cd "$CURRENT_DIR"
mv ../apteDir.nosync/output/data.zip .

echo "Deployment files prepared with data."
echo "To deploy to Fly.io:"
echo "1. Run: fly deploy"
