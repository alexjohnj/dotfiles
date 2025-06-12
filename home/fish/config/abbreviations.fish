# Enable colourised output in ls. This tries to be compatible with GNU ls, BSD
# ls w/ colour support and BSD ls w/o colour support (e.g., OpenBSD).
if ls --color=always &>/dev/null # GNU
    alias ls "ls --color=always"
else if ls -G &>/dev/null # BSD
    alias ls "ls -G"
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
abbr e "emacsclient --no-wait --alternate-editor \"\""
abbr et "emacsclient --tty --alternate-editor \"\""

# Aliases for Beets
alias bean-bal-halifax "bean-report $BEANCOUNT_FILE bal -e Assets:Halifax"
alias bean-bal-assets "bean-report $BEANCOUNT_FILE bal -e Assets:"

set -l YTDL_OUTPUT_FORMAT "%(uploader)s - %(upload_date>%Y-%m-%d)s - %(title)s.%(ext)s"

# Download a YouTube video to the downloads folder
abbr ytdld "yt-dlp -o '~/Downloads/$YTDL_OUTPUT_FORMAT'"

# Download a YouTube video in the current directory
abbr ytdl "yt-dlp -o './$YTDL_OUTPUT_FORMAT'"

# macOS Aliases
abbr dut diskutil
abbr simctl "xcrun simctl"
abbr marked "open -a Marked\ 2"

# Replace default tools with more modern ones
if type -q bat
    abbr cat bat
end

if type -q exa
    alias ls exa
    alias ll "exa --long"
end

if type -q xip
    and not type -q unxip # I have an unxip executable on some systems.
    abbr unxip "xip --expand"
end

if type -q xattr
    abbr sanitize "sudo xattr -r -d com.apple.quarantine"
end

abbr pip "python -m pip"
abbr pip3 "python3 -m pip"

if type -q emulator
    abbr android:boot \
        "emulator -list-avds | fzf --height=~100% --tmux | xargs -I % -L1 emulator -no-snapshot @%"
end
