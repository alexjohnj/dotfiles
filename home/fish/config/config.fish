source "$HOME/.config/fish/exports.fish" # Load Exports
source "$HOME/.config/fish/abbreviations.fish" # Load Aliases

if type -q brew; and test -f (brew --prefix asdf)/libexec/asdf.fish
    # asdf is installed with Homebrew
    source (brew --prefix asdf)/libexec/asdf.fish
else if test -f ~/.asdf/asdf.fish
    # asdf is installed from the git repo.
    source ~/.asdf/asdf.fish
end

if type -q direnv
    direnv hook fish | source
end
