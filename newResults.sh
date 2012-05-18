#!/bin/sh -e
git pull || true
./oldResultsAway.sh
./profileLos.sh
./profileLosJDK.sh
