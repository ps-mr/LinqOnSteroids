#!/bin/bash -e
#git pull
#for ((i=0; i < 40; i++)); do
#  ./profileFB.sh /usr/lib/jvm/jre-1.6.0-openjdk.x86_64/lib/rt.jar > $out 2>&1
#  tail -1 $out >> FB-JDK.csv
#done

out=profileLogJDK-startup-`timestamp`
outCSVOptimized=startup-JDK-optimized.csv
outCSVBaseline=startup-JDK-baseline.csv
nIters=5

gitV=$(git describe --always --dirty --abbrev=40)
echo "Git version: $gitV" > $out

> $outCSVOptimized
> $outCSVBaseline

. javaSettings.inc

for ((i=0; i < $nIters; i++)); do
  {
    $(which time) -f "$gitV;%e;%U;%S;%P" ./start.sh data/rt.jar --onlyOptimized 1 --onlyInFindBugs 1 --debugBench 1
  } >>$out 2>&1
  tail -1 $out >> $outCSVOptimized

  {
    $(which time) -f "$gitV;%e;%U;%S;%P" ./start.sh data/rt.jar --onlyBaseline 1 --onlyInFindBugs 1 --debugBench 1
  } >>$out 2>&1
  tail -1 $out >> $outCSVBaseline
done

rm -f LOSTestLog.csv
