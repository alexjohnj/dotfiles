# Set up the editor
set -gx EDITOR vim

# Set up Homebrew & Casks
if type "brew" > /dev/null
  set -gx HOMEBREW_CASK_OPTS --appdir=/Applications
end

# Set up golang dev environment
set -gx GOPATH $HOME/.go

# Set the PATH
set pathComponents $GOPATH/bin $HOME/.cabal/bin /Applications/MATLAB_R2015a.app/bin /usr/local/share/npm/bin /usr/local/opt/ruby/bin /usr/local/bin /usr/local/sbin /usr/local/lib
for component in $pathComponents[-1..1]
  if test -e $component
    set -gx PATH $component $PATH
  end
end

# Set up node
if type "node" > /dev/null
  set -gx NODE_PATH /usr/local/lib/node
end

# Octave Configuration
set -gx GNUTERM "x11" 

# lpitsa env variables
if type "lpitsa" > /dev/null 2>&1
  set -gx PITSA_PRINTDEF_PATH_ENV /usr/local/bin/pitsa/pltdef/ # Trailing slash is important!
  set -gx PITSA_CONFIG_PATH_ENV /usr/local/bin/pitsa/config/   # Again, slash == important!
  set -gx PITSA_PRINTDEF_NAME_ENV "8X11_landscape.PS"
end
