#!/bin/bash
# Script to clean up temporary files after deployment

echo "Cleaning up temporary deployment files..."

# Remove copied server files
if [ -f "server/serve.js" ]; then
  rm server/serve.js
  echo "Removed server/serve.js"
fi

# Remove copied public files
if [ -f "public/term-viewer-simple.html" ]; then
  rm public/term-viewer-simple.html
  echo "Removed public/term-viewer-simple.html"
fi

if [ -f "public/sanscript.js" ]; then
  rm public/sanscript.js
  echo "Removed public/sanscript.js"
fi

# Remove data.zip if it exists
if [ -f "data.zip" ]; then
  rm data.zip
  echo "Removed data.zip"
fi

# Clean up data directory
if [ -d "data" ]; then
  echo "Cleaning up data directory..."

  # Remove all files in data/sharded
  if [ -d "data/sharded" ]; then
    rm -rf data/sharded/*
    echo "Removed files in data/sharded/"
  fi

  # Remove specific data files
  if [ -f "data/pagemarks.json" ]; then
    rm data/pagemarks.json
    echo "Removed data/pagemarks.json"
  fi

  if [ -f "data/table_new.txt" ]; then
    rm data/table_new.txt
    echo "Removed data/table_new.txt"
  fi

  # Keep the directory structure for future deployments
  echo "Kept data directory structure for future deployments"
fi

echo "Cleanup complete."
echo ""
echo "Note: The application has been deployed and is running with the deployed files."
echo "These local copies were only needed for the deployment process and have been removed."
echo "The data directory structure has been preserved for future deployments."
