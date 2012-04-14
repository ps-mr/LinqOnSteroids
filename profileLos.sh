#!/bin/bash -e
#git pull
nIters=3
sbt compile stage
. javaSettings.inc
for ((i=0; i < $nIters; i++)); do
  out=profileLog-`timestamp`
  { echo -n "Git version: "; git describe --always --dirty; time ./target/start src/test/resources/scalatest-1.6.1.jar; } 2>&1|tee $out
done
cat LOSTestLog.csv >> LOSTestLog-ScalaTest.csv
rm -f LOSTestLog.csv
