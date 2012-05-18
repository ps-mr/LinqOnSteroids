#!/bin/sh -e
git pull || true
./oldResultsAway.sh || true
./profileLos.sh
./profileLosJDK.sh
