if ls --color=always > /dev/null ^&1 # GNU
    alias ls "ls --color=always"
else if ls -G > /dev/null ^&1 # BSD
    alias ls "ls -G"
end
