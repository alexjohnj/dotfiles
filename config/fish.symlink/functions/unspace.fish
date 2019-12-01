function unspace --description "Replace spaces with underscores in the input file names. USAGE: unspace [file ...]"
    for file in $argv
        mv $file (echo $file | tr ' ' '_')
    end
end
