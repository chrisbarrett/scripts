#!/usr/bin/env bash
#
# Migrate all installed ports.
#
# Use this script after upgrading to a new version of OS X.
#
# Source: <https://trac.macports.org/wiki/Migration>

if [ y_or_n "Reinstall Macports and all ports. Continue?" ]; then
    echo aborted.
    exit 1
fi

cd
# save list of ports
port -qv installed > ports.txt
