#!/bin/sh

# This script, based on the parent directory's name `baseName`:
# 1. reads the properties in `baseName.xlsx`
# 2. applies each of the configurations found in to `baseName.xml`
# 3, verifies all properties of all configurations

# Build first the uppx.jar file, e.g., with "sbt assembly" at the project level.

THISPATH=$(dirname "$BASH_SOURCE")
THISFOLDER=$(basename "$THISPATH")

cd "$THISPATH"

java -jar uppx.jar --runAll "$THISFOLDER.xml"

read -n 1 -r -s -p $'Press enter to continue...\n'