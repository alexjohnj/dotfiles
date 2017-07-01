# Set up the editor
set -gx EDITOR vim

# Set up Homebrew & Casks
if type "brew" > /dev/null ^&1
  set -gx HOMEBREW_CASK_OPTS --appdir=/Applications
  set -gx HOMEBREW_NO_ANALYTICS 1
end

# Set up golang dev environment
set -gx GOPATH $HOME/.go

# XZ
set -gx XZ_OPT "--threads=0"

# Set the PATH
set -l path_components \
$HOME/bin \
$HOME/.local/bin \
$HOME/Library/Python/3.6/bin \
$HOME/Library/Python/2.7/bin \
$GOPATH/bin

for c in $path_components[-1..1]
  if begin not contains $c $PATH; and [ -e $c ]; end
    set -x PATH $c $PATH
  end
end

# Set up node
if type "node" > /dev/null ^&1
  set -gx NODE_PATH /usr/local/lib/node
end

# Octave Configuration
set -gx GNUTERM "aqua"

# ledger environment variables
set -gx LEDGER_FILE "$HOME/finance/ledger.journal"

# Set up gpg-agent with SSH. First we try and use the autostart mechanisms in
# GnuPG 2.1. If this isn't available, we make use of an environment file to set
# the appropriate env vars. This assumes gpg-agent has already been started by
# something like launchd.
if gpgconf --launch gpg-agent > /dev/null ^&1
  set -e SSH_AUTH_SOCK
  set -e GPG_TTY
  set -Ux SSH_AUTH_SOCK (gpgconf --list-dir | grep agent-socket | awk -F ':' '{print $2}').ssh
  set -x GPG_TTY (tty)
else if [ -e $HOME/.gpg-agent-info ]
  set -e GPG_AGENT_INFO
  set -e SSH_AUTH_SOCK
  set -e SSH_AGENT_PID
  set -e GPG_TTY
  set -Ux GPG_AGENT_INFO (cat $HOME/.gpg-agent-info | grep GPG_AGENT_INFO | sed 's/.*=//')
  set -Ux SSH_AUTH_SOCK (cat $HOME/.gpg-agent-info | grep SSH_AUTH_SOCK | sed 's/.*=//')
  set -Ux SSH_AGENT_PID (cat $HOME/.gpg-agent-info | grep SSH_AGENT_PID | sed 's/.*=//')
  set -x GPG_TTY (tty)
end
