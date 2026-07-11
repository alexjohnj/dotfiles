# Load home manager session variables
for profile in (string split ' ' -- $NIX_PROFILES)
    if test -f "$profile/etc/profile.d/hm-session-vars.fish"
        source "$profile/etc/profile.d/hm-session-vars.fish"
        break
    end
end

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

if type -q zoxide
    zoxide init fish | source
end
