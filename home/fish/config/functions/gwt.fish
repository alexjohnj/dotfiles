function gwt --description 'Pick a git worktree with fzf and cd into it'
    set -l worktree (git worktree list | fzf --prompt='worktree> ' --no-multi --select-1 --exit-0 | awk '{print $1}')
    if test -n "$worktree"
        cd $worktree
    end
end
