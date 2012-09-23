#!/bin/sh
> tags
find src/ target/ -name '*.scala'|xargs ctags -a
