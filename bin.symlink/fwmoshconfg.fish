#! /usr/bin/env fish

set -l MOSH (which mosh-server)

sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate off
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --add $MOSH
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --unblockapp $MOSH
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on
