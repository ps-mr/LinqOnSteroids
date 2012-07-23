#!/bin/bash
mainJar=$(echo target/*-assembly-*.jar)
java $JAVA_OPTS -jar $mainJar "$@"
