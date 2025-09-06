#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT="$ROOT/releases"

echo "Cleaning generated artifacts under: $OUT"

for dir in "$OUT"/codeuchain-*-v1.0.0; do
  [ -d "$dir" ] || continue
  echo "\n--- Cleaning $dir ---"

  # Remove common generated artifact directories anywhere under the release dir
  find "$dir" -type d \( -name 'node_modules' -o -name 'dist' -o -name '.egg-info' -o -name 'megalinter-reports' -o -name 'obj' -o -name 'bin' -o -name 'build' -o -name '__pycache__' -o -name '.cache' \) -prune -exec rm -rf {} + || true

  # Remove common generated files
  find "$dir" -type f \( -name '*.pyc' -o -name 'coverage.*' -o -name '*.egg-info' -o -name '*.whl' -o -name '*.zip' -o -name '*.tar.gz' \) -delete || true

  # Some language packages produce compiled binaries (dll, exe, pdb) inside bin/ or obj/ folders - already removed above.

  # Recreate archives (overwrite existing)
  base=$(basename "$dir")
  (cd "$OUT" && tar -czf "$base.tar.gz" "$base")
  (cd "$OUT" && zip -r -q "$base.zip" "$base")
  echo "Recreated archives for $base"
done

echo "Cleanup complete."
