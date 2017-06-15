function resetperms -d "Recursively sets directory permissions to 044 and file permissions to 0755 in the current directory"
  find . -type f -exec chmod 0644 '{}' \;
  find . -type d -exec chmod 0755 '{}' \;
end
