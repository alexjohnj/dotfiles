#!/usr/bin/env zsh -f
# Purpose: 	get the RSS feed for a YouTube page
# Inspired By: 	https://eggfreckles.net/notes/youtube-rss/
# Gist: 	https://gist.github.com/tjluoma/fdbc63ceb78a2aecd3d638fd18b6ec6e
#
# From:	Timothy J. Luoma
# Mail:	luomat at gmail dot com
# Date:	2020-01-17; updated 2021-01-10

# 2021-01-10 YouTube currently has both 'rssUrl' and
# 	link rel="alternate" type="application/rss+xml" title="RSS" href="…"
# 	which I do not believe where there a year ago when I first wrote this
# 	I am looking for the latter, and if I don't find it, I fallback to the
# 	former. If neither are found, an error is reported to the user.
#
# 2021-01-10 (Update 2): The previous comment only applies if you are looking
#	at a channel page, i.e. "https://www.youtube.com/512pixels" but NOT if you
#	are looking an an individual video such as 'https://www.youtube.com/watch?v=gQgSdwkvaDg'
#
# Version 2021-01-10.2 -- This script _should_ now work on either of those kinds of pages

NAME="$0:t:r"

if [[ -e "$HOME/.path" ]]
then
	source "$HOME/.path"
else
	PATH="/usr/local/scripts:/usr/local/bin:/usr/bin:/usr/sbin:/sbin:/bin"
fi

COUNT='0'

for URL in "$@"
do

	LINK_REL=$(curl -sfLS "${URL}" \
				| tr '<|>|,|\r' '\n' \
				| awk -F'"' '/application\/rss\+xml/{print $8}')

	if [[ "$LINK_REL" == "" ]]
	then

## Do Not Indent
CHANNEL_ID=$(curl -sfLS "$URL" \
| sed -e 's#<#\
<#g' \
-e 's#>#>\
#g' \
| awk -F'"' '/meta itemprop="channelId"/{print $4}')
## END - Do Not Indent

		if [[ "$CHANNEL_ID" == "" ]]
		then

			echo "$NAME: '\$CHANNEL_ID' and '\$LINK_REL' are both empty for '${URL}'." >>/dev/stderr

			((COUNT++))

			continue

		else

			FEED="https://www.youtube.com/feeds/videos.xml?channel_id=$CHANNEL_ID"

		fi

	else
		FEED="$LINK_REL"
	fi

	echo "$FEED"

			# copy URL to clipboard
	echo -n "$FEED" | pbcopy

done

if [[ "$COUNT" == "0" ]]
then
	# echo "$NAME: No errors"
	exit 0
elif [[ "$COUNT" == "1" ]]
then
	echo "$NAME: One error"
else
	echo "$NAME: $COUNT errors"
fi

exit $COUNT

#EOF
