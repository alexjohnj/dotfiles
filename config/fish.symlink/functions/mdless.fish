function mdless -d "Open a file with mdcat in a pager"
    mdcat $argv | less -r
end
