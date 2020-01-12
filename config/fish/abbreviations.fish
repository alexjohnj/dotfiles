# Enable colourised output in ls. This tries to be compatible with GNU ls, BSD
# ls w/ colour support and BSD ls w/o colour support (e.g., OpenBSD).
if ls --color=always > /dev/null ^&1 # GNU
  alias ls "ls --color=always"
else if ls -G > /dev/null ^&1 # BSD
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
abbr e "emacsclient -n -a \"\""
abbr et "emacsclient -nw -a \"\""

# Aliases for Beets
alias bean-bal-halifax "bean-report $BEANCOUNT_FILE bal -e Assets:Halifax"
alias bean-bal-assets "bean-report $BEANCOUNT_FILE bal -e Assets:"

# Download a YouTube video to the downloads folder
abbr ytdl "youtube-dl -o '~/Downloads/%(title)s.%(ext)s'"

# macOS Aliases
abbr dut "diskutil"
abbr simctl "xcrun simctl"
abbr marked "open -a Marked\ 2"

# Replace default tools with more modern ones
if type "bat" > /dev/null ^&1
    abbr "cat" "bat"
end
