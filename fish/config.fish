# Load oh-my-fish
set -g OMF_PATH $HOME/.local/share/omf
set -g OMF_CONFIG $HOME/.config/omf
source $OMF_PATH/init.fish

set -g theme_display_git yes
set -g theme_display_hg no
set -g theme_display_virtualenv no
set -g theme_display_ruby no
set -g theme_display_user yes
set -g default_user alex

. "$HOME/.config/fish/functions/aliases.fish" # Load Aliases
. "$HOME/.config/fish/functions/exports.fish" # Load Exports
