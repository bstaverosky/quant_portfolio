#!/bin/bash
set -e

REPO_DIR="/home/brian/quant_portfolio"
BRANCH="main"

cd "$REPO_DIR" || { echo "Repo not found: $REPO_DIR"; exit 1; }

# Optional: warn if you have uncommitted changes before pulling
if ! git diff --quiet || ! git diff --cached --quiet; then
  echo "Warning: You have uncommitted changes."
  echo "Commit/stash first to avoid merge issues."
  exit 1
fi

git fetch origin
git pull --rebase origin "$BRANCH"

echo "Pull complete."