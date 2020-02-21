function ytpdl -a OUTPUT_DIR PLAYLIST_URL -d "Download a playlist of videos into a folder"

    if not set -q OUTPUT_DIR[1]
        or not set -q PLAYLIST_URL[1]
        echo "USAGE: ytpdl OUTPUT_DIR PLAYLIST_URL"
        return 1
    end

    set -l FILE_TEMPLATE $OUTPUT_DIR/'%(title)s.%(ext)s'
    set -l ARCHIVE_FILE "$OUTPUT_DIR/archive.txt"

    if not test -d $OUTPUT_DIR
        mkdir -p $OUTPUT_DIR
    end

    youtube-dl -o $FILE_TEMPLATE --download-archive $ARCHIVE_FILE $PLAYLIST_URL
end
