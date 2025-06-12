#!/bin/bash

# Navigate to the directory
cd /home/brian/quant_portfolio || exit

# Add all changes
git add .

# Commit with a timestamp or generic message
git commit -m "Auto-commit on $(date '+%Y-%m-%d %H:%M:%S')"

# Push to the remote repository
git push origin main  # Change 'main' to your branch if it's different
