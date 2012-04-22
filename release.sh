mkdir tarballs
for release in v0.1 v0.2; do
  for extension in gz bz2 xz; do
    git archive --worktree-attributes -o website/tarballs/squopt-$release.tar.xz --prefix=squopt/ $release
  done
done
