function flushdns -d "Flush DNS settings on macOS"
    sudo -p "Password to kill mDNSResponder: " killall -HUP mDNSResponder
end
