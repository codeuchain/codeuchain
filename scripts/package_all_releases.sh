#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT="$ROOT/releases"
mkdir -p "$OUT"

langs=(csharp javascript python go pseudo)
version="v1.0.0"

for lang in "${langs[@]}"; do
  src="$ROOT/packages/$lang"
  if [ -d "$src" ]; then
    dest="$OUT/codeuchain-${lang}-$version"
    echo "Packaging $lang -> $dest"
    rm -rf "$dest"
    mkdir -p "$dest"

    # Copy package contents (exclude common build folders and generated artifacts)
    # Use --prune-empty-dirs to avoid creating empty directories in the destination
    rsync -a \
      --exclude 'build' \
      --exclude '.git' \
      --exclude 'node_modules' \
      --exclude 'bin' \
      --exclude 'obj' \
      --exclude 'dist' \
      --exclude '__pycache__' \
      --exclude '*.pyc' \
      --exclude '*.o' \
      --exclude '*.so' \
      --exclude '*.dll' \
      --exclude '*.pdb' \
      --exclude 'megalinter-reports' \
      --exclude '*.egg-info' \
      --exclude 'coverage*' \
      --exclude '.pytest_cache' \
      --prune-empty-dirs \
      "$src/" "$dest/"

    # Add USAGE.md if not present
    if [ ! -f "$dest/USAGE.md" ]; then
      cat > "$dest/USAGE.md" <<'USAGE'
CodeUChain - Release Package

This release contains only the language-specific package source and examples for quick download.

See the main repository README for language-specific build instructions.
USAGE
    fi

    # Validate dest has content before creating archives
    if [ -z "$(find "$dest" -mindepth 1 -print -quit)" ]; then
      echo "Warning: destination $dest is empty after copying; skipping archive creation."
      continue
    fi

    # Create archives (tar.gz and zip)
    (cd "$OUT" && tar -czf "codeuchain-${lang}-$version.tar.gz" "codeuchain-${lang}-$version")
    (cd "$OUT" && zip -r "codeuchain-${lang}-$version.zip" "codeuchain-${lang}-$version")
  else
    echo "Skipping $lang - package folder not found: $src"
  fi
done

echo "All done. Archives placed in: $OUT"
