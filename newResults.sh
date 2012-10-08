#!/bin/sh -e
git pull || true
./deploy.sh
./oldResultsAway.sh || true
#./profileLos.sh "$@"
./profileLosJDK.sh "$@"
