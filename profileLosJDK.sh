#!/bin/bash -e
#git pull
nIters=3
sbt compile stage
. javaSettings.inc
for ((i=0; i < $nIters; i++)); do
  out=profileLogJDK-`timestamp`
  { echo -n "Git version: "; git describe --always --dirty --abbrev=40; time ./target/start /usr/lib/jvm/jre-1.6.0-openjdk.x86_64/lib/rt.jar; } 2>&1|tee $out
done
cat LOSTestLog.csv >> LOSTestLog-JDK.csv
rm -f LOSTestLog.csv
cat LOSTestLog-raw.csv >> LOSTestLog-JDK-raw.csv
rm -f LOSTestLog-raw.csv
