mkdir -p website/tarballs
for release in v0.1 v0.2; do
  #for ext in gz bz2 xz; do
  for ext in gz; do
    dest=tarballs/squopt-$release.tar.$ext
    #git archive --worktree-attributes -o website/$dest --prefix=squopt/ $release
    size=$(du -h website/$dest|cut -f 1)
    #echo -n "([.tar.$ext]($dest) - $size) "
    echo -n "[.tar.$ext]($dest) - $size "
  done
  echo
done
