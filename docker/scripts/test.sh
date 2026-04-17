#!/bin/bash
find /htcc_work -name "*.s" -type f |\
    while IFS= read -r fname; do
        out_dir=$(dirname "$fname")
        out_file="$out_dir/$(basename "$fname" '.s').o"
        gcc -xassembler -no-pie -o "$out_file" "$fname"
        echo ">>>>> $fname"
        "$out_file"
    done
