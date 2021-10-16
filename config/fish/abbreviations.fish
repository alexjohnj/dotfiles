# Enable colourised output in ls. This tries to be compatible with GNU ls, BSD
# ls w/ colour support and BSD ls w/o colour support (e.g., OpenBSD).
if min_fish_version 3.1.0
    source "$HOME/.config/fish/abbreviations-3.1.0.fish"
else
    source "$HOME/.config/fish/abbreviations-before-3.1.0.fish"
end

# Prevent rm commands from being added to HISTORY. The number of times I've done
# C-p followed by return in my home folder just to run 'rm -rf $SOMETHING' is
# scary. Fortunately that command's never been 'rm -rf *' but it's bound to
# happen someday.
abbr rm " rm"

# Ruby abbreviations
abbr bex "bundle exec"
abbr gem-nuke "gem uninstall -aIx" # Uninstalls all gems

# Copy files with progress information
abbr cpv "rsync -avP"

# Edit with Emacs
abbr e "emacsclient -n -a \"\""
abbr et "emacsclient -nw -a \"\""

# Aliases for Beets
alias bean-bal-halifax "bean-report $BEANCOUNT_FILE bal -e Assets:Halifax"
alias bean-bal-assets "bean-report $BEANCOUNT_FILE bal -e Assets:"

# Download a YouTube video to the downloads folder
abbr ytdl "yt-dlp -o '~/Downloads/%(title)s.%(ext)s'"

# macOS Aliases
abbr dut "diskutil"
abbr simctl "xcrun simctl"
abbr marked "open -a Marked\ 2"

# Replace default tools with more modern ones
if type -q "bat"
    abbr "cat" "bat"
end

if type -q "exa"
    alias ls "exa"
    alias ll "exa --long"
end
