#!/bin/bash
# Script to prepare for deploying new code WITH new data

echo "Cleaning up from previous deployments..."
./cleanup.sh

echo "Preparing deployment with new data..."

# Create necessary directories
mkdir -p server public data

# Copy server files
cp ../../web/server/serve.js server/

# Create public directory structure
mkdir -p public/js

# Copy static files
cp ../../web/public/index.html public/
cp ../../web/public/js/sanscript.js public/js/

# Create the data.zip file with real data
echo "Creating data.zip with real dictionary data..."
# Save current directory
CURRENT_DIR=$(pwd)
cd ../../data/output
/usr/bin/zip -vr data.zip pagemarks.json table_new.txt sharded -X
# Return to original directory
cd "$CURRENT_DIR"
mv ../../data/output/data.zip .

echo "Deployment files prepared with data."
echo "To deploy to Fly.io:"
echo "1. Run: fly deploy"
