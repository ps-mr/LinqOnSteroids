mkdir -p website/tarballs
for release in v0.1 v0.2; do
  for ext in gz bz2 xz; do
    git archive --worktree-attributes -o website/tarballs/squopt-$release.tar.$ext --prefix=squopt/ $release
  done
done
