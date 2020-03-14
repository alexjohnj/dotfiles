function min_fish_version -a TEST_VERSION
    set -l input "$TEST_VERSION"\n"$FISH_VERSION"
    echo $input | sort -CV -
    return $status
end
