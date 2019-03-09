function initsync -d "Initialise the global stignore file in the current directory"
    ln -i -s "$HOME/.stglobalignore" .
    echo '#include .stglobalignore' > '.stignore'
end
