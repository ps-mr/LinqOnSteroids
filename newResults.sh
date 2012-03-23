#!/bin/bash -e
#git pull
nIters=3
for ((i=0; i < $nIters; i++)); do
  out=testLog-`timestamp`
  { echo -n "Git version: "; git describe --always; time sbt 'test-only performancetests.*'; } 2>&1|tee $out
done
