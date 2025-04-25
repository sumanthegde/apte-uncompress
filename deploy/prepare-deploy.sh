#!/bin/bash
# Script to prepare the project for deployment

# Create necessary directories
mkdir -p deploy/server deploy/public deploy/data/sharded

# Copy server files with modifications
cp ../serve.js server/

# Copy static files
cp ../term-viewer-simple.html public/
cp ../sanscript.js public/

# Copy deployment configuration
# (These files should already exist in the deploy directory)

echo "Deployment files prepared in the 'deploy' directory."
echo "To deploy to Fly.io:"
echo "1. cd deploy"
echo "2. fly launch (first time only)"
echo "3. fly volumes create apte_data --size 3 (first time only)"
echo "4. fly deploy"
echo "5. Upload data files using SFTP (see README for instructions)"
