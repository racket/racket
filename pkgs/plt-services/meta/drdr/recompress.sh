#!/bin/bash

unset a i
while IFS= read -r -d $'\0' file; do
    newfile=$(dirname "${file}")/$(basename "${file}" .tgz).tar.7z
    if [ -f "${newfile}" ] ; then
        rm -f "${newfile}"
    fi
    7z x "${file}" -so | 7z a "${newfile}" -t7z -m0=lzma -mfb=64 -ms=on -mx=9 -si && rm -f ${file}
done < <(find . -name '*.tgz' -print0)

