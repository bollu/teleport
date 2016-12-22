 #!/bin/bash
 # teleport.sh
function tp() {
    # $@ takes all arguments of the shell script and passes it along to `teleport-exe
    # which is our tool
    OUTPUT=`teleport-exe $@`
    # return code 2 tells the shell script to cd to whatever `teleport` outputs
    if [ $? -eq 2 ]
        then cd "$OUTPUT"
        else echo "$OUTPUT"
    fi
}

fpath=(`pwd` $fpath) 
