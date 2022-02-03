#!/bin/bash

EXEC_INVALID="Invalid executable file."
DIR_INVALID="Invalid directory."
USAGE="Usage: executable test_dir"

VFLAGS=$"--error-exitcode=15 --leak-check=full --show-leak-kinds=all \
--errors-for-leak-kinds=all"

PASSED=$"\e[32;01mPASSED\e[0m"
FAILED=$"\e[31;01mFAILED\e[0m"
OK=$"\e[32;01mOK\e[0m"
ERROR=$"\e[31;01mERROR\e[0m"
CODE_0=$"\e[32;01m$?\e[0m"
CODE_OTHER=$"\e[31;01m$?\e[0m"
BREAK=$'\n'

if [ $# == 2 ]; then 
	EXEC=$1
	DIR=$2
else
	echo $USAGE
	exit 0
fi

if [ -f $EXEC ]; then
	EXEC="./$EXEC"
else 
	echo $EXEC_INVALID
	exit 0
fi

if [ ! -d "$DIR" ]; then
	echo $DIR_INVALID
	exit 0
fi

for f in "$DIR"/*.in; do
	F_DISPLAY="\e[1m${f##*/}\e[0m"
	
	echo -e $F_DISPLAY

    FNAME="${f%.in}"

    for o in "${FNAME}".out; do
    	valgrind $VFLAGS --log-file="vlg" $EXEC $f $GEN 1> out 2>&1
        
        echo -n "Test result: "

        if diff out $o > /dev/null 2>&1; then
        	echo -e $PASSED
        else
        	echo -e $FAILED
        fi

        echo -n "Return code: " 
        if [ $? == 0 ]; then
                echo -e $CODE_0
        else 
                echo -e $CODE_OTHER
        fi

        echo -n "Memory check: "

	if grep -q "ERROR SUMMARY: 0 errors" vlg; then
                echo -e $OK
	else
                echo -e $ERROR
	fi

    done
	
	echo $BREAK
done

rm -f vlg
rm -f out
