function mdless -d "Open a file with mdcat in a pager"
    mdcat --columns 120 $argv | less -r
end
