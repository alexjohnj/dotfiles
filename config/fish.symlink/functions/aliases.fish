# Enable colourised output in ls. This tries to be compatible with GNU
# ls, BSD ls w/ colour support and BSD ls w/o colour support (e.g.,
# OpenBSD).
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

# Copy files with progress information
alias cpv "rsync -avP"

# MATLAB Aliases
alias matlab "matlab -nodesktop"

abbr julia "julia -q"

# Edit with Emacs
alias e "emacsclient -n -a \"\""
alias et "emacsclient -nw -a \"\""

# Work with encrypted ledger files
alias eledger "gpg --batch -d -q $LEDGER_FILE | ledger -f - "
alias ehledger "gpg --batch -d -q $LEDGER_FILE | hledger -f -"
alias done "osascript -e 'display notification with title \"Done\"'"
abbr ledger "ledger --strict"

# Aliases for Beets
abbr beet-flac2alac "beet convert -k -f alac -d . format:FLAC"
alias bean-bal-halifax "bean-report $BEANCOUNT_FILE bal -e Assets:Halifax"
alias bean-bal-assets "bean-report $BEANCOUNT_FILE bal -e Assets:"
