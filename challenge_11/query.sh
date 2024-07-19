#!/usr/bin/env bash

# Regex Query Tool
# Allow the user to enter a text string and then in a separate text box enter a regex pattern. It will run the regular expression against the string and return any matches or flag errors in the regular expression.

tmp=/tmp/re-text.txt
touch $tmp

if [ -z $EDITOR ]; then
	EDITOR=nano
fi

: | fzf --bind "ctrl-e:execute($EDITOR $tmp),enter:put(\\n)" \
	--preview "rg --pcre2 --passthru -e {q} --color=always <$tmp | bat --style grid,numbers" \
	--preview-window up,99% \
	--header "Ctrl-e to edit text"

