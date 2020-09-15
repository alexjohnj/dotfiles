function notify -d "Display a system notification"
    set -l description "A command has finished running."
    set -l title "Command Finished"

    osascript -e "display notification \"$description\" with title \"$title\""
end
