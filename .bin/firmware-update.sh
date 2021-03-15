#!/bin/sh
fwupdmgr refresh >> /dev/null 2>&1

updates=$(fwupdmgr get-updates 2> /dev/null | grep -c "Updatable")

if [ "$updates" -gt 0 ]; then
    echo "罹 $updates  "
else
    echo ""
fi
