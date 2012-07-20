#!/bin/sh -e
git pull || true
./deploy.sh
./oldResultsAway.sh
./profileLos.sh
./profileLosJDK.sh
