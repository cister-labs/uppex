#!/bin/sh

# This script, based on the parent directory's name `baseName`:
# 1. reads the properties in `baseName.xlsx`
# 2. applies each of the configurations found in to `baseName.xml`
# 3, verifies all properties of all configurations

# Build first the uppex.jar file, e.g., with "sbt assembly" at the project level.

NAME="simple-with-conf"
UPPEX_JAR="../../target/scala-3.0.2/uppex.jar"

cd "$THISPATH"
echo "running: java -jar $UPPEX_JAR --runAll \"$NAME.xml\""

java -jar $UPPEX_JAR --runAll "$NAME.xml"

read -n 1 -r -s -p $'Press enter to continue...\n'