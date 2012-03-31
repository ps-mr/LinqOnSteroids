#!/bin/bash -e
#git pull
nIters=3
for ((i=0; i < $nIters; i++)); do
  out=profileLog-`timestamp`
  { echo -n "Git version: "; git describe --always; time sbt "test:run lib/scalatest-1.6.1.jar"; } 2>&1|tee $out
done