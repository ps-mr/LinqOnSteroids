#!/bin/sh
> tags
find src target sampleapp -name '*.scala'|xargs ctags -a
