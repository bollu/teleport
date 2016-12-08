#!/bin/bash
# teleport.sh

function tp() {
    OUTPUT=`teleport-exe $@`
    # return code 2 is used to indicate that the shell script
    # should use the output to warp to
    if [ $? -eq 2 ]
        then cd "$OUTPUT"
        else echo "$OUTPUT"
    fi
}

fpath=(`pwd` $fpath) 
