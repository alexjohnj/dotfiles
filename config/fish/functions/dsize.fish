# Shortcut for the `du` command with human readable output, sorting
# and a grand total when used with multiple files.
function dsize -d "A shortcut for du -csh | sort -rh"
  if [ (count $argv) -le 1 ]
    du -sh $argv | sort -rh
  else
    du -csh $argv | sort -rh
  end
end
