# cd Shortcuts
alias dropdev "cd ~/Dropbox/Documents/Development/"
alias alexjohnj "cd ~/Dropbox/Documents/Development/Web/alexjohnj/"
alias geographyas "cd ~/Dropbox/Documents/Development/Web/geographyas/"

# Copy files with progress information
alias cpv "rsync -avP"

# So my University requires that you connect via its VPN to SSH into the 
# school's computers. This requires using a POS client that can not be quit
# by killing the associated process because it restarts itself. Thus, these 
# functions are necessary. Big thanks to @mevanlc for finding this.
# (https://gist.github.com/Andrewpk/7558715#comment-1209618) 
function startleedsvpn
  sudo launchctl load -w /Library/LaunchDaemons/net.juniper.AccessService.plist
  launchctl load -w /Library/LaunchAgents/net.juniper.pulsetray.plist
  echo "Don't forget to run killleedsvpn to kill this POS..."
end

function killleedsvpn
  launchctl unload -w /Library/LaunchAgents/net.juniper.pulsetray.plist
  sudo launchctl unload -w /Library/LaunchDaemons/net.juniper.AccessService.plist
  osascript -e 'tell application "Junos Pulse" to quit'
end

