# Set up the editor
set -gx EDITOR vim

# Set up golang dev environment
set -gx GOPATH $HOME/.go

# Set up node
if type "node" > /dev/null
  set -gx NODE_PATH /usr/local/lib/node
end

# Set up Homebrew & Casks
if type "brew" > /dev/null
  set -gx HOMEBREW_CASK_OPTS --appdir=/Applications
end

# Octave Configuration
set -gx GNUTERM "x11" 

# Set the PATH
set pathComponents $GOPATH/bin /Applications/MATLAB_R2014a.app/bin/maci64 /usr/local/share/npm/bin /usr/local/opt/ruby/bin /usr/local/bin /usr/local/sbin /usr/local/lib

for component in $pathComponents[-1..1]
  if test -e $component
    set -gx PATH $component $PATH
  end
end
