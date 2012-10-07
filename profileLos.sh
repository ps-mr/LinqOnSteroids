#!/bin/bash -e
#git pull
nIters=3
. javaSettings.inc
for ((i=0; i < $nIters; i++)); do
  out=profileLog-`timestamp`
  { echo -n "Git version: "; git describe --always --dirty --abbrev=40; time ./start.sh --executionCycles 10 src/test/resources/scalatest-1.6.1.jar "$@"; } 2>&1|tee $out
  cat LOSTestLog.csv >> LOSTestLog-ScalaTest.csv
  rm -f LOSTestLog.csv
  cat LOSTestLog-raw.csv >> LOSTestLog-ScalaTest-raw.csv
  rm -f LOSTestLog-raw.csv
done
