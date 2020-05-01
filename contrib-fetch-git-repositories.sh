#!/bin/sh

# Visits all git repositories within $MYDIR and executes "git pull origin master"

set -o nounset  ## Error, when referencing undefined variable
set -o errexit  ## Exit on any error

MYDIR="$HOME/.emacs.d/contrib"
cd "${MYDIR}"
find . -type d -name ".git" | while read folder;
do
    cd "${folder}"  ## switch to .git folder
    cd ..           ## visit parent folder which is the repo main folder
    echo "| ${folder} | 1 before pull |" $(git log --pretty=format:'%h | %ci |' -n 1)
    git pull origin master
    echo "| ${folder} | 2 after pull  |" $(git log --pretty=format:'%h | %ci |' -n 1)
    cd "${MYDIR}"
done

#end
