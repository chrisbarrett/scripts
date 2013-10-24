#!/usr/bin/env bash
#
# Migrate all installed ports.
#
# Use this script after upgrading to a new version of OS X.
#
# Source: <https://trac.macports.org/wiki/Migration>

if [ y_or_n "WARNING: MacPorts and all ports will be reinstalled. Continue?" ]; then
    echo aborted.
    exit 1
fi
cd
# Save list of ports.
port -qv installed > ports.txt
# Uninstall all ports.
port -f uninstall installed
# Clean any partially-completed builds.
port clean all
# Restore scripts.
curl -O https://svn.macports.org/repository/macports/contrib/restore_ports/restore_ports.tcl
chmod +x restore_ports.tcl
sudo ./restore_ports.tcl ports.txt
