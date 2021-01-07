#!/bin/bash
find /htcc_work -name "*.s" -type f |\
    while read fname; do 
        gcc -xassembler -no-pie -o "$(dirname $fname)/$(basename $fname '.s').o" $fname
        echo ">>>>> $fname"
        ./$(dirname $fname)/$(basename $fname ".s").o
    done
