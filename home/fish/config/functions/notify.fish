function notify -d "Display a system notification" -a "command_name"
    set -l og_status $status
    if test -z $command_name
        set command_name "A command"
    end

    if test $og_status -eq 0
        set -f title "Command Succeeded"
        set -f description "$command_name finished successfully"
    else
        set -f title "Command Failed"
        set -f description "$command_name failed with status code $og_status"
    end

    osascript -e "display notification \"$description\" with title \"$title\""

    return $og_status
end
