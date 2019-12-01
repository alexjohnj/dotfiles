function bak -d "move file to .bak version"
    set -l filename $argv[1]
    mv $filename $filename".bak"
end
