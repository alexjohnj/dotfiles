#!/usr/bin/env bash

##########################
# UI Stuff
##########################

# Save to iCloud rather than the disk in save panels
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool true

# Keep the printer app open when all jobs are finished
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool false

# Enable Automatic Termination for all apps
defaults write NSGlobalDomain NSDisableAutomaticTermination -bool false

# Check for updates weekly
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 7

# Enable Smart Dashes
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool true

# Enable Smart Quotes
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool true

# Disable Key Repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool true

# Revert to default key repeat speed
defaults delete NSGlobalDomain KeyRepeat

##########################
# Finder
##########################

# Search the entire hard drive by default
defaults delete com.apple.finder FXDefaultSearchScope

# Hide the ~/Library folder
chflags hidden ~/Library

# Enable the damn warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool true

# Enable column view for new finder windows by default
defaults write com.apple.finder FXPreferredViewStyle -string "clmv"

# Disable AirDrop on all but WiFi interfaces
defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool false

##########################
# The Dock
##########################

# Move the dock to the bottomg of the screen
defaults write com.apple.dock orientation bottom

# Disable Auto hiding the dock
defaults write com.apple.dock autohide -bool false

# Revert dock icon size to default
defaults delete com.apple.dock tilesize

##########################
# Safari
##########################

# Disable Safari's dev tools
defaults write com.apple.Safari IncludeDevelopMenu -bool false
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool false
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool false

# Enable opening safe files when downloaded
defaults write com.apple.Safari AutoOpenSafeDownloads -bool true

##########################
# Spotlight
##########################

# Enable indexing of external drives
sudo -p "Enter the admin's password to enable Spotlight indexing of external drives:" defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array ""

# Restart anything that needs restarting
for app in "Dock" "Finder"
do
	killAll "$app"
done

printf "Done. You may need to log out and back in to enable all changes.\n"
