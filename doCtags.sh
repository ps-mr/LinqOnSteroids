#!/bin/sh
find sampleapp src eval macros rfp target -name '*.scala'|xargs ctags -a -f tags.tmp
mv -f tags.tmp tags
