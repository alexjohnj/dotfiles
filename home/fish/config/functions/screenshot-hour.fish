function screenshot-hour -d "Set iOS simulator status bar's time to the correct screenshot time"
    xcrun simctl status_bar booted override --time 09:41 --operatorName ''
end
