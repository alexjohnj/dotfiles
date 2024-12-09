function pnpmf
    if set -q PNPMF_FILTER_PREFIX
        set -f filter (string join "" $PNPMF_FILTER_PREFIX $argv[1])
    else
        set -f filter $argv[1]
    end

    set -l arguments $argv[2..]

    pnpm --filter $filter $arguments
end
