function notify -d "Display a desktop notification via the terminal" -a "command_name"
    set -l og_status $status
    if test -z "$command_name"
        set command_name "A command"
    end

    if test $og_status -eq 0
        set -f title "Command Succeeded"
        set -f description "$command_name finished successfully"
    else
        set -f title "Command Failed"
        set -f description "$command_name failed with status code $og_status"
    end

    # OSC 777 fields are separated by ";", so strip it and control characters.
    set title (string replace -ra '[;\x00-\x1f\x7f]' ' ' -- $title)
    set description (string replace -ra '[;\x00-\x1f\x7f]' ' ' -- $description)

    if isatty stdout
        printf '\e]777;notify;%s;%s\e\\' $title $description
    end

    return $og_status
end
