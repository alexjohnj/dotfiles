function rmirror -a SOURCE DESTINATION -d "Mirror the contents of SOURCE in DESTINATION"
    rsync -ah --delete --progress $SOURCE $DESTINATION
end
