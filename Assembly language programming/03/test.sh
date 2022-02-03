#!/bin/bash

EXEC_INVALID="Invalid executable file."
DIR_INVALID="Invalid directory."
USAGE="Usage: executable test_dir"

PASSED=$"\e[32;01mPASSED\e[0m"
FAILED=$"\e[31;01mFAILED\e[0m"
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

for f in "$DIR"/*.ppm; do
    FILE="${f##*/}"
    if [[ $FILE == "Y"* ]]; then
        continue
    fi

    FILE_DISPLAY="\e[1m${FILE}\e[0m"
	echo -e $FILE_DISPLAY

    NAME="${FILE%.ppm}"
    for o in "$DIR"/Y"$NAME"*.ppm; do
        REST="${o#*_}"
        REST="${REST%.*}"
        COMPONENT="${REST%%_*}"
        VAL="${REST##*_}"
        TEST_FLAG="1"

        echo -e "Component: \e[1m${COMPONENT}\e[0m, value: \e[1m${VAL}\e[0m"

    	$EXEC $f $COMPONENT $VAL $TEST_FLAG 1> out 2>&1
        
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
    done
	
	echo $BREAK
done


rm -f out