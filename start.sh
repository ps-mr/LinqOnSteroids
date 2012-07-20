#!/bin/bash
mainJar=$(echo target/*-assembly-*.jar)
java -jar $mainJar "$@"
