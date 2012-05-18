#!/bin/sh -x
for i in ScalaTest JDK; do
  for j in '' -raw; do
    mv LOSTestLog-$i$j.csv away/LOSTestLog-$i$j-`timestamp`.csv
  done
done
