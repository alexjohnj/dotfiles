# Set up the editor
set -gx EDITOR vim

# Add PKG_PATH for OpenBSD
if [ (uname -s) = "OpenBSD" ]
  # Alberta mirror
  set -gx PKG_PATH "http://ftp.openbsd.org/pub/OpenBSD/"(uname -r)"/packages/"(uname -p)"/"
end

# Set up Homebrew & Casks
if type "brew" > /dev/null ^&1
  set -gx HOMEBREW_CASK_OPTS --appdir=/Applications
  set -gx HOMEBREW_NO_ANALYTICS 1
end

# Set up golang dev environment
set -gx GOPATH $HOME/.go

# Set the PATH
set -l path_components \
$HOME/bin \
$HOME/.local/bin \
$GOPATH/bin \
/Applications/MATLAB_R2016a.app/bin \
/usr/local/bin/taup/bin

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

# Set up Seismic Unix
set -l su_path /usr/local/bin/su
if [ -e $su_path ]
  set -gx CWPROOT $su_path
  set -x PATH $su_path/bin $PATH
end

# Set up SAC
if [ -e /usr/local/bin/sac ]
  set -gx SACHOME /usr/local/bin/sac
  set -gx SACAUX $SACHOME/aux
  set -x PATH $PATH $SACHOME/bin
end

# Set up gpg-agent with SSH
if [ -S $HOME/.gnupg/S.gpg-agent ]
  set -gx GPG_AGENT_INFO $HOME/.gnupg/S.gpg-agent
end
if [ -S $HOME/.gnupg/S.gpg-agent.ssh ]
  set -gx SSH_AUTH_SOCK $HOME/.gnupg/S.gpg-agent.ssh
end
set -gx GPG_TTY (tty)
