# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Theme
set fish_theme agnoster

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Example format: set fish_plugins autojump bundler

# Path to your custom folder (default path is $FISH/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish
# . "$HOME/.config/fish/functions/set_prompt.fish" # Load prompt
. "$HOME/.config/fish/functions/aliases.fish" # Load Aliases
. "$HOME/.config/fish/functions/exports.fish" # Load Exports

# Fish Greeting
function fish_greeting -d "Fish Greeting"
  set packageCount (brew outdated | wc -l | tr -d ' ')

  if test $packageCount -eq 1
    echo $packageCount "Package can be updated" 
  else if test $packageCount -gt 1
    echo $packageCount "Pakages can be updated"
  end
end
