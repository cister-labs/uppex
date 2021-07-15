#!/bin/sh

# This script, based on the parent directory's name `baseName`:
# 1. reads the properties in `baseName.xlsx` and applies them to `baseName.xml`
# 2. opens Uppaal with file named `baseName.xml`

# Build first the uppx.jar file, e.g., with "sbt assembly" at the project level.

THISPATH=$(dirname "$BASH_SOURCE")
THISFOLDER=$(basename "$THISPATH")
UPPAAL_JAR=~/Applications/uppaal64-4.1.24/uppaal.jar
UPPX_JAR="../../target/scala-3.0.0/uppx.jar"

cd "$THISPATH"

# -- 1 --
java -Xss16m -jar $UPPX_JAR "$THISFOLDER"

read -n 1 -r -s -p $'Press enter to continue...\n'

# -- 2 --
java -jar $UPPAAL_JAR "$THISPATH/$THISFOLDER".xml &