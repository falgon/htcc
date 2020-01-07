<h1><p align="center">htcc</a></h1>

<p align="center">
:baby_chick: A tiny C language compiler (x86-64)
</p>

<div align="center">
<a href="https://travis-ci.org/falgon/htcc">
    <img src="https://travis-ci.org/falgon/htcc.svg?branch=master" alt="Build Status" />
</a>
<img src="https://img.shields.io/badge/architecture-amd64-blue.svg" alt="Architecture" />
<a href="./LICENSE">
<img src="https://img.shields.io/badge/License-BSD%203--Clause-blue.svg" alt="License" />
</a>
</div>

## Build

```sh
$ stack build
$ stack build --fast # no optimized
```

## Usage

```sh
$ stack exec htcc -- -h
Usage: htcc [--visualize-ast] [--img-resolution RESOLUTION] file [-o|--out file]
            [-w|--supress-warns]

Available options:
  -h,--help                Show this help text
  --visualize-ast          Visualize an AST built from source code
  --img-resolution RESOLUTION
                           Specify the resolution of the AST graph to be
                           generated (default: 640x480)
  file                     Specify the input file name
  -o,--out file            Specify the output destination file name, supported
                           only svg (default: ./out.svg)
  -w,--supress-warns       Disable all warning messages
```

Simple compilation:

```sh
$ echo 'int printf(); int main() { printf("hello world!\n"); }' > t.c
$ stack exec htcc -- t.c > t.s
$ gcc -no-pie t.c -o out
```

For one liner:

```sh
$ echo 'int printf(); int main() { printf("hello world!\n"); }' | stack exec htcc -- /dev/stdin | gcc -xassembler -no-pie -o out -  
```

## AST diagram generation

htcc has the ability to visualize ASTs built from loaded C code.
This option allows to specify the resolution and output file.
Examples are shown in the following table.

<table>
<tr>
<th>Command</th>
<th>Output</th>
</tr>
<tr>
    <td><pre>$ echo 'int main() { return 1 * 2 + 4; }' &#124;\
    stack exec htcc -- /dev/stdin --visualize-ast</pre></td>
    <td><img src="./assets/example_ast/calc.png" alt="AST graph of the some calculation"></td>
</tr>
<tr>
    <td><pre>$ echo 'int printf();
    void fizzbuzz(int n) { 
        for (int i = 1; i < n; ++i) { 
            if (!(i % 15)) printf("fizzbuzz\n"); 
            else if (!(i % 3)) printf("fizz\n"); 
            else if (!(i % 5)) printf("buzz\n"); 
            else printf("%d\n", i); 
        } 
    } 
    int main() { fizzbuzz(50); }' &#124;\
    stack exec htcc -- /dev/stdin\
        --visualize-ast\
        --img-resolution 1280x720\
        --out fizzbuzz.svg</pre></td>
    <td><img src="./assets/example_ast/fizzbuzz.png" alt="AST graph of FizzBuzz"></td>
</tr>
</table>

## Test

```sh
$ stack test --test-arguments help
htcc> test (suite: htcc-test, args: help)

--test-arguments are available by:
        inc     : Test itself with test code written in C (default, more faster).
        subp    : Given C codes as input, run HUnit tests.

htcc> Test suite htcc-test failed
Test suite failure for package htcc-0.0.0.1
    htcc-test:  exited with: ExitFailure 1
Logs printed to console

$ stack test
$ stack test --test-arguments inc
$ stack test --test-arguments subp
```

## Benchmark

```sh
$ stack bench
```

## About emoji of commit messages

The emoji included in the commit message is used according to [gitmoji](https://gitmoji.carloscuesta.me/).

## FAQ

### Your compiler is inefficient :)

I know :confused:

This is a compiler made for research, not for practical purposes
and the author also developed the compiler for the first time.
If you can suggest improvements, please submit issues or send PRs.
Thanks in advance for all the improvements.

### When I try to play with ghci, I get a warning "WARNING:. is owned by someone else, IGNORING!".

Check your permissions. 
The answer on [stack overflow](https://stackoverflow.com/questions/24665531/ghci-haskell-compiler-error-home-user-ghci-is-owned-by-someone-else-ignor) may be useful.

## References

* [N1570 - JTC1/SC22/WG14](http://open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf): C11 working draft (PDF)
* [N1570 - JTC1/SC22/WG14](https://port70.net/~nsz/c/c11/n1570.html): C11 working draft (HTML)
