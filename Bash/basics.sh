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

Fruits=('Apple' 'Banana' 'Oranges')
echo ${Fruits[1]}

function giveMeArgs() {
    echo $#
    echo $*
    echo $@
}

echo $(giveMeArgs "ay" "nay" "oke")

echo ${Fruits[@]}

Fruits+=('Watermelon')
echo ${Fruits[@]}

Fruits=("${Fruits[@]}" "${Veggies[@]}")
echo ${Fruits[@]}

### Dictionaries

declare -A sounds

sounds[dog]="bark"
sounds[cow]="moo"
sounds[bird]="tweet"

echo ${sounds[@]}
echo ${#sounds[@]}

for val in "${sounds[@]}"; do
  echo $val
done


echo "something"

a=6
echo $((a + 200)) 
echo $((RANDOM%=200))

function caseStatement() {
    case "$1" in
        start | up)
            echo "starting or upping"
            ;;

        *)
            echo "Usage: $0 {start|stop|ssh}"
            ;;
    esac   
}

echo $(caseStatement stop)

printf "Hello %s, I'm %s\n" Sven Olga
printf "1 + 1 = %d\n" 2
printf "This is how you print a float: %f\n" 2


## Getting options


function gettingOptions() {
 while [[ "$1" =~ ^- && ! "$1" == "--" ]]; do case $1 in
  -V | --version )
    echo "version"
    exit
    ;;
  -s | --string )
    shift; string=$1
    ;;
  -f | --flag )
    flag=1
    echo "flagg"
    ;;
esac; shift; done
if [[ "$1" == '--' ]]; then shift; fi
}

echo $(gettingOptions --flag)

if ping -c 1 google.com; then
  echo "It appears you have a working internet connection"
fi

if grep -q 'foo' ~/.bash_history; then
  echo "You appear to have typed 'foo' in the past"
fi






