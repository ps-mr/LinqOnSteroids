#!/bin/sh -e
git pull || true
./profileLos.sh
./profileLosJDK.sh
