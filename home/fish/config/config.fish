source "$HOME/.config/fish/exports.fish" # Load Exports
source "$HOME/.config/fish/abbreviations.fish" # Load Aliases

if type -q mise
    mise activate fish | source
end

if type -q direnv
    direnv hook fish | source
end

if type -q fzf
    fzf --fish | source
end
