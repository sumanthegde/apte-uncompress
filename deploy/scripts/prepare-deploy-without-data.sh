#!/bin/bash
# Script to prepare for deploying new code WITHOUT new data

echo "Cleaning up from previous deployments..."
./cleanup.sh

echo "Preparing deployment without new data..."

# Create necessary directories
mkdir -p server public data

# Copy server files
cp ../../web/server/serve.js server/

# Create public directory structure
mkdir -p public/js

# Copy static files
cp ../../web/public/index.html public/
cp ../../web/public/js/sanscript.js public/js/

# Create a dummy data.zip file
echo "Creating dummy data.zip file..."
mkdir .dummy_folder
/usr/bin/zip -vr data.zip .dummy_folder -X
rmdir .dummy_folder

echo "Deployment files prepared without data."
echo "To deploy to Fly.io:"
echo "1. Run: fly deploy"
