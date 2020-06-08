function swap -d "Swap two files" -a the_file other_file
    mv $the_file $the_file".tmp"
    mv $other_file $the_file
    mv $the_file".tmp" $other_file
end
