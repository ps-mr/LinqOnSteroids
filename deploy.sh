#!/bin/bash
if [ $(ls target/*-assembly-*.jar|wc -l) != 1 ]; then
  rm target/*-assembly-*.jar
fi
sbt assembly
