#!/bin/sh

TEST_FILE="$1"
AIN_FILE="$(mktemp --suffix=.ain)"

printf "Running test $TEST_FILE... "

if ! sys4c -o "$AIN_FILE" "$TEST_FILE"; then
    echo compile failed
    rm -f "$AIN_FILE"
    exit 1
fi

if ! xsystem4 --nodebug "$AIN_FILE"; then
    echo execution failed
    rm -f "$AIN_FILE"
    exit 1
fi

echo passed
rm -f "$AIN_FILE"
