#!/usr/bin/env fish

function log
  logger -s -t "[mail-sync]" $argv
end

log "Mail sync started."

# Log folder
if not test -d "/Users/alex/Library/Logs/get-mail"
  mkdir -p "/Users/alex/Library/Logs/get-mail"
end

# Test for network connectivity
if not ping -Q -c 1 -W 1 google.com > /dev/null ^&1
  log "No network connection. Skipping this sync."
  exit 1
end

if pgrep mbsync
  log "mbsync already in process. Killing..."
  pkill mbsync
end

log "Starting mbsync..."
/usr/local/bin/mbsync -a -q
if test $status -ne 0
  osascript -e 'display notification "From get-mail.sh" with title "Error fetching mail"'
end
log "Starting mu..."
/usr/local/bin/mu index -q --maildir=/Users/alex/.mail
log "Mail sync finished."
