#!/usr/bin/env bash

set -euo pipefail

### Variables

NAME="Kajetan"

echo $NAME
echo "$NAME"
echo "${NAME}!"


echo ''

### Quotes

echo "hello $NAME"
echo 'hello $NAME'

ls && ls -al
ls || ls -al

### Functions

get_name() {
    echo "Kajetan"
}

echo "You are $(get_name)"


echo "I'm in $(pwd)"
echo "I'm in `pwd`"

### Conditionals

STR="something"

if [[ -z "$STR" ]]; then
    echo "String is empty"
elif [[ -n "$STR" ]]; then
    echo "String is not empty"
fi

### Parameter expansions

echo ${NAME}
echo ${NAME/K/k}
echo ${NAME:0:2}
echo ${NAME::2}
echo ${NAME::-1}
echo ${OKAY:-Cake}

STRX="/path/to/foo.cpp"
echo ${STRX%.cpp}
echo ${STRX%.cpp}.o
echo ${STRX##*.} # Extension
echo ${STRX##*/} # Base path

echo ${#STRX}
echo ${STRX:=val}
echo ${STRX:+val}
echo ${STRX:?val} # Show the message and exit if STRX not set

other="All Right"
echo ${other}
echo ${other,}
echo ${other,,}

other_low="all right"
echo ${other_low}
echo ${other_low^}
echo ${other_low^^}

### Loops

for i in /home/kajetan/Documents/*; do
    echo $i
done

for ((i = 0 ; i < 4 ; i++)); do
    echo $i
done

for i in {1..5}; do
    echo "Welcome $i"
done

for i in {5..8..5}; do
    echo "Welcome $i"
done

cat example | while read line; do
    echo $line
done

### Other functions

function myFunc() {
    echo "Hello $1"
}

function otherFunc() {
    echo "Hello there"
}

myFunc "Johnn"

result="$(otherFunc)"

function numFunc() {
    return 1
}

if numFunc; then
    echo "success"
else
    echo "failure"
fi

### Arrays

Fruits=('Apple', 'Banana', 'Oranges')
echo ${Fruits[1]}



