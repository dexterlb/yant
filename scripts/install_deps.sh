#!/bin/sh
if [ package.json -ot node_modules/marker ]; then
    echo "nothing to update"
else
    ( cd ../js/qtrp-potoo && npm install && npm run build ) || exit 1
    npm install && touch node_modules/marker
fi
