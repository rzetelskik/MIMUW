#!/bin/bash
if [[ "$#" -gt 1 ]]; then
    FILE=$1;
else 
    exit 1
fi

if ! [[ -f "$FILE" ]]; then
    exit 1
fi

#check arguments' correctness 
for arg in "${@:2}"; do
    if ! [[ "$arg" -gt 0 && "$arg" -lt 1000 ]] ; then
        exit 1
    fi
done

for arg in "${@:2}"; do
    line=$(grep "^${arg}" "$FILE")

    if [[ -n $line ]]; then
        i=3
        total=0

        #get every third number (separated by semicolons) in a line 
        sections=$(echo $line | cut -d';' -f $i)

        while [ -n "$sections" ]; do
            (( total += $sections ))
            (( i += 3 ))

            sections=$(echo $line| cut -d';' -f $i)
        done

        echo "$arg"\;"$total"
    fi
done

exit 0