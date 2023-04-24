#!/bin/sh

# This script,:
# 1. reads the properties in `$NAME.xlsx` and applies them to `$NAME.xml`
# 2. opens Uppaal with file named `$NAME.xml`

# Build first the uppex.jar file, e.g., with "sbt assembly" at the project level.

NAME="simple"
UPPAAL_JAR=~/Applications/uppaal64-4.1.24/uppaal.jar
UPPX_JAR="../../target/scala-3.0.2/uppex.jar"

cd "$THISPATH"

# -- 1 --
java -Xss16m -jar $UPPX_JAR "$NAME"

read -n 1 -r -s -p $'Press enter to continue...\n'

# -- 2 --
java -jar $UPPAAL_JAR "$NAME".xml &