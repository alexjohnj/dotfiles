#!/usr/bin/env bash

##########################
# General Stuff
##########################

# Save to disk rather than iCloud in save panels
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Quit the printer app when all jobs are done
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# Disable Automatic Termination for all apps
defaults write NSGlobalDomain NSDisableAutomaticTermination -bool true

# Check for updates daily
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

# Enable smart dashes
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# Disable Smart Quotes
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# Enable Key Repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Increase Key repeat speed
defaults write NSGlobalDomain KeyRepeat -int 0

##########################
# Finder
##########################

# Search the current directory by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Show the ~/Library folder
chflags nohidden ~/Library

# Disable the damn warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Enable list view for new finder windows by default
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

# Enable AirDrop for all network interfaces
defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true

##########################
# The Dock
##########################

# Move the dock to the left of the screen
defaults write com.apple.dock orientation left

# Auto hide the dock
defaults write com.apple.dock autohide -bool true

# Set Dock Icon Size to 40px
defaults write com.apple.dock tilesize -int 40
# Remove the delay from automatically hiding/showing the dock
defaults write com.apple.dock autohide-delay -int 0

# Disable resizing of the dock (without using the above command)
defaults write com.apple.dock size-immutable -int 1

##########################
# Safari
##########################

# Enable Safari's dev tools
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true

# Disable opening safe files when downloaded
defaults write com.apple.Safari AutoOpenSafeDownloads -bool false

##########################
# Spotlight
##########################

# Disable indexing of external drives
sudo -p "Enter the admin's password to disable Spotlight indexing of external drives:" defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array "/Volumes"

# Restart anything that needs restarting
for app in "Dock" "Finder"
do
  killAll "$app"
done

printf "Done. You may need to log out and back in to enable all changes.\n"
