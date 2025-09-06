#!/usr/bin/env bash
set -euo pipefail

# Upload release assets from the local `releases/` folder to a GitHub Release using the `gh` CLI.
# Usage: ./scripts/upload_release_assets.sh <tag>
# Example: ./scripts/upload_release_assets.sh v1.0.0

if ! command -v gh >/dev/null 2>&1; then
  echo "gh CLI not found. Install from https://cli.github.com/ and authenticate (gh auth login)." >&2
  exit 1
fi

TAG="${1:-}"
if [ -z "$TAG" ]; then
  echo "Usage: $0 <tag>" >&2
  exit 2
fi

REPO="$(git remote get-url origin 2>/dev/null || echo '')"
if [ -z "$REPO" ]; then
  echo "Cannot determine git remote 'origin' url. Run this from the repository root." >&2
  exit 3
fi

ASSETS_DIR="releases"
if [ ! -d "$ASSETS_DIR" ]; then
  echo "Assets directory '$ASSETS_DIR' not found." >&2
  exit 4
fi

echo "Ensuring release '$TAG' exists..."
if ! gh release view "$TAG" >/dev/null 2>&1; then
  echo "Release $TAG does not exist on GitHub; creating a draft release and then uploading assets.";
  gh release create "$TAG" --title "$TAG" --notes "Release $TAG" || true
fi

shopt -s nullglob
uploaded=0
for asset in "$ASSETS_DIR"/codeuchain-*"$TAG"*.zip "$ASSETS_DIR"/codeuchain-*"$TAG"*.tar.gz; do
  if [ -f "$asset" ]; then
    echo "Uploading: $asset"
    gh release upload "$TAG" "$asset" --clobber
    uploaded=$((uploaded+1))
  fi
done

if [ "$uploaded" -eq 0 ]; then
  echo "No assets matched for tag '$TAG' in $ASSETS_DIR" >&2
  exit 6
fi

echo "Uploaded $uploaded assets to release $TAG." 
