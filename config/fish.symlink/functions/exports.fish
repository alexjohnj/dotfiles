# Set up the editor
set -gx EDITOR vim

# Set up Homebrew & Casks
if type "brew" > /dev/null ^&1
  set -gx HOMEBREW_CASK_OPTS --appdir=/Applications
  set -gx HOMEBREW_NO_ANALYTICS 1
end

# Set up golang dev environment
set -gx GOPATH $HOME/.go

# Ruby environment
set -gx GEM_HOME "$HOME/.gem"

# XZ
set -gx XZ_OPT "--threads=0"

# Set the PATH
set -l path_components \
$HOME/bin \
$HOME/.local/bin \
$HOME/Library/Python/3.6/bin \
$HOME/Library/Python/2.7/bin \
$GEM_HOME/bin \
$GOPATH/bin \
/usr/local/sbin

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
set -gx BEANCOUNT_FILE "$HOME/finance/ledger.beancount"
