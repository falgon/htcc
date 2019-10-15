# htcc

A tiny C language compiler (x86-64).

## currently features

- [x] numerical calculation
- [ ] statements
    - [x] return statement
    - [x] `if` statement
    - [x] `else` statement
    - [x] `while` statement
    - [x] `for` statement
    - [ ] `do` statement
    - [ ] `continue` statement
- [x] block scope
- [x] array (multidimensional array)
- [x] function call/definition (up to 6 parameters)
- [ ] types
    - [x] `int`
    - [x] pointer
    - [x] `char`
    - [ ] `float`
- [x] struct (with tag)

and will add more some features...

## build

```sh
$ stack build
```

## test

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

## benchmark

```sh
$ stack bench
```
