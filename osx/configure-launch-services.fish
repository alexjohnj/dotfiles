#!/usr/bin/env fish

if not command --search --query duti
	echo "ERROR: duti must be installed."
	exit 1
end

set TEXT_EDITOR_BUNDLE_ID "org.gnu.Emacs"
set VIDEO_PLAYER_BUNDLE_ID "com.colliderli.iina"
set AUDIO_PLAYER_BUNDLE_ID "com.colliderli.iina"

function associate -a bundle_id file_identifier
    duti -s $bundle_id $file_identifier all
end

# Text Editing
set TEXT_EDITOR_UTIS \
    "com.apple.property-list" \
    "com.netscape.javascript-source" \
    "net.daringfireball.markdown" \
    "org.tug.tex" \
    "public.c-header" \
    "public.css" \
    "public.data" \
    "public.json" \
    "public.objective-c-source" \
    "public.plain-text" \
    "public.python-script" \
    "public.ruby-script" \
    "public.shell-script" \
    "public.source-code" \
    "public.swift-source" \
    "public.text" \
    "public.yaml" \

set TEXT_EDITOR_EXTENSIONS \
    "fish" \
    "rs" \

for extension in $TEXT_EDITOR_EXTENSIONS
    associate $TEXT_EDITOR_BUNDLE_ID $extension
end

for uti in $TEXT_EDITOR_UTIS
    associate $TEXT_EDITOR_BUNDLE_ID $uti
end

# Video Playback
set VIDEO_FILE_UTIS \
    "public.movie" \
    "public.mpeg-4" \
    "com.apple.m4v-video" \

set VIDEO_FILE_EXTENSIONS \
    "mkv"

for uti in $VIDEO_FILE_UTIS
    associate $VIDEO_PLAYER_BUNDLE_ID $uti
end

for extension in $VIDEO_FILE_EXTENSIONS
    associate $VIDEO_PLAYER_BUNDLE_ID $extension
end

# Audio Playback
set AUDIO_FILE_UTIS \
    "public.mp3" \
    "com.apple.m4a-audio" \
    "org.xiph.flac" \

for uti in $AUDIO_FILE_UTIS
    associate $AUDIO_PLAYER_BUNDLE_ID $uti
end
