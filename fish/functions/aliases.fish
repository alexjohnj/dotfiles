# cd Shortcuts
alias dropdev "cd ~/Dropbox/Documents/Development/"
alias alexjohnj "cd ~/Dropbox/Documents/Development/Web/alexjohnj/"
alias geographyas "cd ~/Dropbox/Documents/Development/Web/geographyas/"

# ls Aliases
if ls --version 2>/dev/null | grep -q 'coreutils'
    alias ls "ls --color=always"
else
    alias ls "ls -G"
end

# Copy files with progress information
alias cpv "rsync -avP"

# MATLAB Aliases
alias matlab "matlab -nodesktop"

# Edit with Emacs
alias e "emacsclient -n -a \"\""
alias et "emacsclient -nw -a \"\""

# Work with encrypted ledger files
alias eledger "gpg --batch -d -q $LEDGER_FILE | ledger -f - "
alias ehledger "gpg --batch -d -q $LEDGER_FILE | hledger -f -"
