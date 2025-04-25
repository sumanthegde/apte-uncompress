#!/bin/bash
# Script to upload sample data to the server

# Create a tar archive of the sample data
echo "Creating sample data archive..."
tar -czf sample-data.tar.gz data/

# Upload the sample data to the server
echo "Uploading sample data to the server..."
fly ssh sftp -a apte-dictionary put sample-data.tar.gz /tmp/

# Extract the sample data on the server
echo "Extracting sample data on the server..."
fly ssh -a apte-dictionary "cd /app && tar -xzf /tmp/sample-data.tar.gz"

# Restart the app
echo "Restarting the app..."
fly apps restart apte-dictionary

echo "Sample data uploaded and app restarted."
