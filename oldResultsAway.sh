#!/bin/sh
now=`timestamp`
for i in -ScalaTest -JDK ''; do
  for j in '' -raw; do
    name=LOSTestLog$i$j
    src=$name.csv
    [ -f $src ] && mv $src away/$name-$now.csv
  done
done
for name in startup-JDK-baseline startup-JDK-optimized; do
    src=$name.csv
    [ -f $src ] && mv $src away/$name-$now.csv
done
