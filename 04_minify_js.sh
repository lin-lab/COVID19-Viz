#!/bin/bash
set -euxo pipefail

curl -X POST -s --data-urlencode 'input@www/leaflet_recolor.js' \
  https://javascript-minifier.com/raw > www/leaflet_recolor.min.js
