function beet-flac2alac -d "Convert all FLAC files in beets library to ALAC saving originals to the cwd"
    beet convert -k -f alac -d . format:FLAC
end
