source "$HOME/.config/fish/exports.fish" # Load Exports
source "$HOME/.config/fish/abbreviations.fish" # Load Aliases

if type -q nix; and test -f ~/.nix-profile/share/asdf-vm/asdf.fish
    # asdf is installed through nix. This can be done with home-manager but
    # would overwrite all my fish config files.
    source ~/.nix-profile/share/asdf-vm/asdf.fish
else if type -q brew; and test -f (brew --prefix asdf)/libexec/asdf.fish
    # asdf is installed with Homebrew
    source (brew --prefix asdf)/libexec/asdf.fish
else if test -f ~/.asdf/asdf.fish
    # asdf is installed from the git repo.
    source ~/.asdf/asdf.fish
end

if type -q direnv
    direnv hook fish | source
end
