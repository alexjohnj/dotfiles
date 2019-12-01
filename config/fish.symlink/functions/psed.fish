function psed -d "Run Perl in sed mode. psed [regexp] [file ...]"
    if test (count $argv) -lt 1
        printf "Missing regular expression. USAGE: psed [regexp] [file ...]\n"
        return 1
    end

    set -l regexp $argv[1]

    if test (count $argv) -eq 1
        # Reading from stdin
        perl -p -e $regexp
    else
        perl -pi -e $regexp $argv[2..-1]
    end
end
