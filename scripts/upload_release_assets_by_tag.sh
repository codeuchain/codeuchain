#!/usr/bin/env bash
set -euo pipefail

# Upload release assets into per-language release tags.
# It inspects files in `releases/` named like `codeuchain-<lang>-<tag>.zip` and uploads
# them to GitHub Releases named `<short>/<tag>` where `short` is a short language prefix.
# Examples:
#  codeuchain-python-v1.0.0.zip -> tag: py/v1.0.0
#  codeuchain-javascript-v1.0.0.zip -> tag: js/v1.0.0

if ! command -v gh >/dev/null 2>&1; then
  echo "gh CLI not found. Install and authenticate (gh auth login)." >&2
  exit 1
fi

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ASSETS_DIR="$REPO_ROOT/releases"

get_prefix() {
  case "$1" in
    python) echo py ;;
    javascript|js) echo js ;;
    cpp|c++) echo cpp ;;
    csharp) echo csharp ;;
    go) echo go ;;
    pseudo) echo pseudo ;;
    *) echo "$1" ;;
  esac
}

shopt -s nullglob
files=("$ASSETS_DIR"/*.zip "$ASSETS_DIR"/*.tar.gz)
if [ ${#files[@]} -eq 0 ]; then
  echo "No assets found in $ASSETS_DIR" >&2
  exit 2
fi

uploaded_total=0
for f in "${files[@]}"; do
  base=$(basename "$f")
  # Expect names like codeuchain-<lang>-v1.0.0.zip
  if [[ "$base" =~ ^codeuchain-([a-zA-Z0-9_+-]+)-([vV][0-9].*)\.(zip|tar.gz)$ ]]; then
    lang="${BASH_REMATCH[1]}"
    ver="${BASH_REMATCH[2]}"
  else
    echo "Skipping unrecognized file: $base" >&2
    continue
  fi

  key="$lang"
  # normalize some names (replace + with - and lowercase)
  key="${key//+/-}"
  # lowercase by using awk to be portable
  key="$(echo "$key" | awk '{print tolower($0)}')"

  short="$(get_prefix "$key")"
  tag="$short/$ver"

  echo "Processing $base -> release tag: $tag"

  if gh release view "$tag" >/dev/null 2>&1; then
    echo "Release $tag exists; uploading $base"
  else
    echo "Release $tag does not exist; creating (draft=false)"
    gh release create "$tag" --title "$tag" --notes "Release for $tag" || true
  fi

  gh release upload "$tag" "$f" --clobber
  uploaded_total=$((uploaded_total+1))
done

echo "Uploaded $uploaded_total assets."
