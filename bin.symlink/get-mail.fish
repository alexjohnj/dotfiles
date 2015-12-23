#!/usr/bin/env fish

# Make a folder for logs from launchd
if not test -d "/Users/alex/Library/Logs/get-mail"
  mkdir -p "/Users/alex/Library/Logs/get-mail"
end

if pgrep mbsync
  pkill mbsync
end

/usr/local/bin/mbsync -V -a
/usr/local/bin/mu index -q --maildir=/Users/alex/.mail
