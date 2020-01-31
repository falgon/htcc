#!/bin/bash

set -eux

echo 'int main() { return 1 * 2 + 4; }' | stack exec htcc -- /dev/stdin --visualize-ast 
printf 'int printf(); 
    void fizzbuzz(int n) { 
        for (int i = 1; i < n; ++i) { 
            if (!(i % 15)) printf("fizzbuzz\n");
            else if (!(i % 5)) printf("fizz\n");
            else if (!(i % 3)) printf("buzz\n"); 
            else printf("%d\n", i); 
        }
    } 
    int main() { fizzbuzz(50); }' |\
        stack exec htcc -- /dev/stdin \
            --visualize-ast \
            --img-resolution 1280x720 \
            --out fizzbuzz.svg

if [ ! -f ./out.svg ] || [ ! -f ./fizzbuzz.svg ] ; then exit 1; fi
if [ "640" != "$(identify -format '%w' out.svg)" ]; then exit 1; fi
if [ "480" != "$(identify -format '%h' out.svg)" ]; then exit 1; fi
if [ "1280" != "$(identify -format '%w' fizzbuzz.svg)" ]; then exit 1; fi
if [ "720" != "$(identify -format '%h' fizzbuzz.svg)" ]; then exit 1; fi
