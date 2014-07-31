# Set up the editor
set -gx EDITOR vim

# Set up golang dev environment
set -gx GOPATH $HOME/.go

# Set up node
set -gx NODE_PATH /usr/local/lib/node

# Set up Homebrew & Casks
set -gx HOMEBREW_CASK_OPTS --appdir=/Applications

# Octave Configuration
set -gx GNUTERM "x11" 

# Set the PATH
set -gx PATH $GOPATH/bin /usr/local/share/npm/bin /usr/local/opt/ruby/bin /usr/local/sbin /usr/local/bin $PATH
