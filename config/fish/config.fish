source "$HOME/.config/fish/exports.fish" # Load Exports
source "$HOME/.config/fish/abbreviations.fish" # Load Aliases

if type -q "brew"
    source (brew --prefix asdf)/libexec/asdf.fish
end
