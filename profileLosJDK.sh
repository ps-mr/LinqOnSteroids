#!/bin/bash -e
#git pull
DATA=data/rt.jar
nIters=1
. javaSettings.inc
for ((i=0; i < $nIters; i++)); do
  out=profileLogJDK-`timestamp`
  { echo -n "Git version: "; git describe --always --dirty --abbrev=40; time ./start.sh $DATA "$@"; } 2>&1|tee $out
done
cat LOSTestLog.csv >> LOSTestLog-JDK.csv
rm -f LOSTestLog.csv
cat LOSTestLog-raw.csv >> LOSTestLog-JDK-raw.csv
rm -f LOSTestLog-raw.csv
