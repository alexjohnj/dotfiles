function resetperms -d "Recursively sets directory permissions to 0644 and file permissions to 0755"
  if [ (count $argv) -lt 1 ]
    set TARGET (pwd)
  else
    set TARGET $argv
  end

  find "$TARGET" -type f -exec chmod 0644 '{}' \;
  find "$TARGET" -type d -exec chmod 0755 '{}' \;
end
