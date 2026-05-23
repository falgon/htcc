#!/bin/bash

set -euo pipefail

work_dir=${HTCC_WORK_DIR:-/htcc_work}
asm_list=$(mktemp)
count=0
trap 'rm -f "$asm_list"' EXIT

find "$work_dir" -name "*.s" -type f -print0 > "$asm_list"

while IFS= read -r -d '' fname; do
    count=$((count + 1))
    out_dir=${fname%/*}
    base=${fname##*/}
    out_file="$out_dir/${base%.s}.o"
    gcc -xassembler -no-pie -o "$out_file" "$fname"
    echo ">>>>> $fname"
    "$out_file"
done < "$asm_list"

if [ "$count" -eq 0 ]; then
    echo "no assembly files found in $work_dir" >&2
    exit 1
fi
