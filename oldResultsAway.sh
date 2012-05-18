#!/bin/sh -x
now=`timestamp`
for i in ScalaTest JDK; do
  for j in '' -raw; do
    mv LOSTestLog-$i$j.csv away/LOSTestLog-$i$j-$now.csv
  done
done
