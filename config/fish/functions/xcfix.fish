function xcfix -d "Fix Xcode by nuking DerivedData"
    set -l DERIVED_DATA "$HOME/Library/Developer/Xcode/DerivedData"

    if not test -d $DERIVED_DATA
        return 0
    end

    rm -rf $DERIVED_DATA
end
