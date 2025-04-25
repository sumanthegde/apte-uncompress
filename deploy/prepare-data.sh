#!/bin/bash
# Script to prepare data for deployment

# Check if we're in the right directory
if [ ! -d "../apteDir.nosync/output" ]; then
  echo "Error: apteDir.nosync/output directory not found."
  echo "Please run this script from the deploy directory."
  exit 1
fi

# Verify table_new.txt format
echo "Verifying table_new.txt format..."
head -n 5 ../apteDir.nosync/output/table_new.txt
echo ""
echo "If the format looks incorrect (should be colon-separated), please fix it before deploying."
echo ""

# Convert table_new.txt format if needed
if [ "$1" == "--convert-format" ]; then
  echo "Converting table_new.txt format..."
  # For tab to colon conversion
  sed 's/\t/:/g' ../apteDir.nosync/output/table_new.txt > ../apteDir.nosync/output/table_new.txt.new
  mv ../apteDir.nosync/output/table_new.txt.new ../apteDir.nosync/output/table_new.txt
  echo "Conversion complete."
fi

# Create a zip file of the data
echo "Creating data archive..."
/usr/bin/zip -vr data.zip ../apteDir.nosync/output/pagemarks.json ../apteDir.nosync/output/table_new.txt ../apteDir.nosync/output/sharded -X

echo "Data archive created successfully."
echo "You can now run 'fly deploy' to deploy the application."
