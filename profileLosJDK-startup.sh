#!/bin/bash -e
#git pull
#for ((i=0; i < 40; i++)); do
#  ./profileFB.sh /usr/lib/jvm/jre-1.6.0-openjdk.x86_64/lib/rt.jar > $out 2>&1
#  tail -1 $out >> FB-JDK.csv
#done

out=profileLogJDK-startup-`timestamp`
outCSV=startup-JDK.csv
nIters=40

gitV=$(git describe --always --dirty)
echo "Git version: $gitV" > $out
> $outCSV

sbt compile stage
for ((i=0; i < $nIters; i++)); do
  {
    $(which time) -f "$gitV;%e;%U;%S;%P" ./target/start /usr/lib/jvm/jre-1.6.0-openjdk.x86_64/lib/rt.jar
  } >>$out 2>&1
  tail -1 $out >> $outCSV
done
rm -f LOSTestLog.csv
