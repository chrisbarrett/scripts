#!/usr/bin/env bash
#
# Migrate all installed ports.
#
# Use this script after upgrading to a new version of OS X.
#
# Source: <https://trac.macports.org/wiki/Migration>

cd
# save list of ports
port -qv installed > ports.txt
