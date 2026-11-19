function gco --description 'Checkout a branch, or cd to the worktree that has it checked out'
    if test (count $argv) -ne 1
        echo "usage: gco <branch>" >&2
        return 1
    end

    set -l branch $argv[1]
    set -l target (git worktree list --porcelain | awk -v ref="refs/heads/$branch" '
        /^worktree / { path = substr($0, 10) }
        $0 == "branch " ref { print path; exit }
    ')

    if test -n "$target"; and test "$target" != (git rev-parse --show-toplevel)
        cd $target
    else
        git checkout $branch
    end
end
