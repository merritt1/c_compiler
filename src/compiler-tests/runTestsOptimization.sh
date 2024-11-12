#!/bin/bash

PROJECT_ROOT='.'
INPUT_FILES="$PROJECT_ROOT/optimizations/input/*.txt"
EXPECTED_FILES_PATH="$PROJECT_ROOT/optimizations/expected"
COMPILER_EXEC="$PROJECT_ROOT/../compiler/compiler -s ir -O 3"

OUTPUT_FILES="optimizations/optimizations-output"
mkdir -p $OUTPUT_FILES
rm -f "$OUTPUT_FILES/*"

errorcount=0
filecount=0

for f in $INPUT_FILES
do
    name=${f##*/}
    output="$OUTPUT_FILES/$name"
    expected="$EXPECTED_FILES_PATH/$name"
    $COMPILER_EXEC <$f >$output
    ((filecount++))
    if diff $output $expected >/dev/null; then
        rm -f $output
    else
        ((errorcount++))
        echo "error with $f"
        echo "+++++++++++++++++++++++++++++++++++++++++++++++" >> error.log
        echo "failed test case $name" >> error.log
        echo "---------------------------" >> error.log
        echo "test case source" >> error.log
        echo "---------------------------" >> error.log
        cat $f >> error.log
        echo >> error.log
        echo "===========================" >> error.log
        echo "actual output" >> error.log
        echo "===========================" >> error.log
        cat $output >> error.log
        echo >> error.log
        echo "***************************" >> error.log
        echo "expected output" >> error.log
        echo "***************************" >> error.log
        cat $expected >> error.log
    fi
done
    echo "Optimizations have $errorcount errors out of $filecount"
    if test "$errorcount" -eq "0"
    then
        rm -r $OUTPUT_FILES
    fi
