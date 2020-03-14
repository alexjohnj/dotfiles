if ls --color=always &> /dev/null # GNU
    alias ls "ls --color=always"
else if ls -G &> /dev/null # BSD
    alias ls "ls -G"
end
