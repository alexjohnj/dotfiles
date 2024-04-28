#!/usr/bin/env fish

if test (count $argv) -lt 1
	echo "USAGE: set-launchservices-editor EDITOR_BUNDLE_ID"
	exit 1
end

if not command -s -q duti
	echo "ERROR: duti must be installed."
	exit 1
end

set -l EDITOR_BUNDLE_ID $argv[1]
set UTIS "public.text"
set EXTENSIONS \
	c \
	css \
	fish \
	h \
	hs \
	html \
	jl \
	js \
	json \
	m \
	markdown \
	md \
	plist \
	py \
	rb \
	scss \
	sh \
	swift \
	toml \
	tex \
	txt \
	yaml \
	yml \

for extension in $EXTENSIONS
  duti -s $EDITOR_BUNDLE_ID $extension all
end

for uti in $UTIS
  duti -s $EDITOR_BUNDLE_ID $uti all
end
