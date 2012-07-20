#!/bin/sh
now=`timestamp`
for i in -ScalaTest -JDK ''; do
  for j in '' -raw; do
    src=LOSTestLog$i$j.csv
    [ -f $src ] && mv $src away/LOSTestLog$i$j-$now.csv
  done
done
