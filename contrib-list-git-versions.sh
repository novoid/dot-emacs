#!/bin/sh

# Visits all git repositories within $MYDIR and prints git version numbers:

MYDIR="$HOME/.emacs.d/contrib"
cd "${MYDIR}"
find . -type d -name ".git" | while read folder;
do
    cd "${folder}"
    echo "| ${folder} |" $(git log --pretty=format:'%h | %ci |' -n 1)
    cd "${MYDIR}"
done

#end
