#!/bin/sh

FILE_WITH_EL_FILES="compile-el-files.tmp"

## generate list of *.el files for compilation:

rm ${FILE_WITH_EL_FILES}

for file in *.el;do echo "(byte-compile-file \"/home/vk/.emacs.d/$file\")" >> ${FILE_WITH_EL_FILES}; done

for file in contrib/*.el;do echo "(byte-compile-file \"/home/vk/.emacs.d/$file\")" >> ${FILE_WITH_EL_FILES}; done

## compile *.el files and org-mode:

/usr/bin/emacs -batch -l ${FILE_WITH_EL_FILES} -kill && \
cd contrib/org-mode/ && \
make clean && make && make autoloads && make doc

#end
