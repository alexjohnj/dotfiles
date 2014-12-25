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
  # This needs some sort of service to check for updates every couple of hours
  # and put the number of updates into a file called .homebrew-outdated. 
  # Personally, I use a lunchd service to get the job done.
  if [ -e $HOME/.homebrew-outdated ]
    set packageCount (cat $HOME/.homebrew-outdated)
    if [ $packageCount -eq 1 ]
      printf "1 Package can be updated\n"
    else if [ $packageCount -gt 1 ]
      printf "%d Packages can be updated\n" $packageCount
    end
  end
end
