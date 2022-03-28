function xcfix -d "Fix Xcode by nuking DerivedData"
    set -l DERIVED_DATA "$HOME/Library/Developer/Xcode/DerivedData"
    set -l CACHE_DIR "$HOME/Library/Caches/com.apple.dt.Xcode"
    set -l CLANG_MODULE_CACHE_DIR "$TMPDIR/../C/clang/ModuleCache".
    set -l APPLICATION_STATE "$HOME/Library/Saved Application State/com.apple.dt.Xcode.savedState"

    rm -rf $DERIVED_DATA
    rm -rf $CACHE_DIR
    rm -rf $CLANG_MODULE_CACHE_DIR
    rm -rf $APPLICATION_STATE
end
