# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish
. $fish_path/oh-my-fish.fish

# Theme Configuraiton
if [ $TERM = "eterm-color" ]
   Theme l
else
   Theme bobthefish
 end

set -g theme_display_git yes
set -g theme_display_hg no
set -g theme_display_virtualenv no
set -g theme_display_ruby no
set -g theme_display_user yes
set -g default_user alex

. "$HOME/.config/fish/functions/aliases.fish" # Load Aliases
. "$HOME/.config/fish/functions/exports.fish" # Load Exports
