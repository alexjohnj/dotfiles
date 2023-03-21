source "$HOME/.config/fish/exports.fish" # Load Exports
source "$HOME/.config/fish/abbreviations.fish" # Load Aliases

if type -q "brew"; and test -f (brew --prefix asdf)/libexec/asdf.fish
    source (brew --prefix asdf)/libexec/asdf.fish
end
