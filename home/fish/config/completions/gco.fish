complete -c gco -f -a '(git for-each-ref --format="%(refname:short)" refs/heads refs/remotes 2>/dev/null | string match -v "*/HEAD" | string match -v origin)'
