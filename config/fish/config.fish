source "$HOME/.config/fish/exports.fish" # Load Exports
source "$HOME/.config/fish/abbreviations.fish" # Load Aliases


set -l ASDF_CONFIG_FILE "/usr/local/opt/asdf/asdf.fish"
if test -f $ASDF_CONFIG_FILE
    source $ASDF_CONFIG_FILE
end
