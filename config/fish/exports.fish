# Set up the editor
set -gx EDITOR vim

# Set up Homebrew & Casks
set -gx HOMEBREW_CASK_OPTS --appdir=/Applications
set -gx HOMEBREW_NO_ANALYTICS 1

# Set up golang dev environment
set -gx GOPATH $HOME/.go

# Ruby environment
set -gx GEM_HOME "$HOME/.gem"

# XZ
set -gx XZ_OPT "--threads=0"

# Dart and Flutter environment
set -gx PUB_CACHE $HOME/.pub-cache

# Set the PATH
set -l path_components \
    $HOME/bin \
    $HOME/.local/bin \
    $HOME/.cargo/bin \
    $PUB_CACHE/bin \
    $HOME/Library/Python/3.7/bin \
    $HOME/Library/Python/2.7/bin \
    $GEM_HOME/bin \
    $GOPATH/bin \
    /opt/local/bin \
    /opt/local/sbin \
    /usr/local/sbin

for c in $path_components[-1..1]
    if begin not contains $c $PATH; and [ -e $c ]; end
        set -x PATH $c $PATH
    end
end

# Set up node
set -gx NODE_PATH /usr/local/lib/node

# Octave Configuration
set -gx GNUTERM "aqua"

# ledger environment variables
set -gx LEDGER_FILE "$HOME/finance/ledger.journal"
set -gx BEANCOUNT_FILE "$HOME/finance/ledger.beancount"

# bat Configuration

set -x BAT_STYLE "plain"
set -x BAT_THEME "base16"
