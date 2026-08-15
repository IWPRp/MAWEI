#!/usr/bin/env bash
# Double-click launcher for the MAWEI web dashboard (macOS).
#
# The dashboard reads the Sankey JSON with fetch(), which browsers block on file:// URLs
# for security reasons. So it cannot be opened by double-clicking index.html; it has to be
# served over http. This script does that against the live outputs/ tree, so whatever the
# R pipeline last wrote is what appears.
#
# The static interface/MAWEI.html needs none of this and still opens directly.

set -euo pipefail
cd "$(dirname "$0")"

PORT=8787

# The dashboard expects the artefacts at ./data. A symlink keeps a single copy on disk and
# means a pipeline rerun is picked up with no rebuild step. http.server follows symlinks.
if [ ! -e data ]; then
  ln -s ../outputs/files data
fi

if [ ! -e data/manifest.json ]; then
  echo "No artefacts found at outputs/files/manifest.json."
  echo "Run the pipeline first:  Rscript R/flows_energy_water.R"
  read -r -p "Press return to close."
  exit 1
fi

# Reuse an already-running server rather than failing on a bound port.
if ! curl -s -o /dev/null "http://localhost:${PORT}/index.html"; then
  python3 -m http.server "${PORT}" --bind 127.0.0.1 >/dev/null 2>&1 &
  sleep 1
fi

open "http://localhost:${PORT}/index.html"
echo "MAWEI dashboard: http://localhost:${PORT}/index.html"
echo "Close this window to stop the server."
wait
