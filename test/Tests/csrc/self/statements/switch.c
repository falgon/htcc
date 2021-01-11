// This is a c compiler test file. This comment itself is a line comment test.
/*
 * This comment is also a block comment test.
 */

int printf();
int exit();
int test_num;

int assert(long expected, long actual, char* code)
{
    if (expected == actual) {
        printf("[OK]:statements/switch test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:statements/switch test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int main()
{
    printf(">>>> tests: statements/switch\n");
    test_num = 1;

    assert(42, ({ int i = 0; switch (0) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; }), "({ int i = 0; switch (0) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; })");
    assert(43, ({ int i = 0; switch (1) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; }), "({ int i = 0; switch (1) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; })");
    assert(44, ({ int i = 0; switch (2) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; }), "({ int i = 0; switch (2) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; })");
    assert(42, ({ int i = 42; switch (3) { case 0: i = 5; break; case 1: i = 6; break; case 2: i = 7; break; } i; }), "({ int i = 42; switch (3) { case 0: i = 5; break; case 1: i = 6; break; case 2: i = 7; break; } i; })");
    assert(42, ({ int i = 0; switch (0) { case 0: i = 42; break; default: i = 43; } i; }), "({ int i = 0; switch (0) { case 0: i = 42; break; default: i = 43; } i; })");
    assert(43, ({ int i = 0; switch (1) { case 0: i = 42; break; default: i = 43; } i; }), "({ int i = 0; switch (0) { case 0: i = 42; break; default: i = 43; } i; })");
    assert(42, ({ int i = 0; switch (1) { case 0: 0; case 1: 0; case 2: 0; i = 42; } i; }), "({ int i = 0; switch (1) { case 0: 0; case 1: 0; case 2: 0; i = 42; } i; })");
    assert(0, ({ int i = 0; switch (3) { case 0: 0; case 1: 0; case 2: 0; i = 42; } i; }), "({ int i = 0; switch (3) { case 0: 0; case 1: 0; case 2: 0; i = 42; } i; })");
    assert(42, ({ int i = 40; switch (0) { case 0: ++i; case 1: ++i; } i; }), "({ int i = 40; switch (0) { case 0: ++i; case 1: ++i; } i; })");
    assert(41, ({ int i = 40; switch (i) { case 20 * 2: ++i; } i; }), "({ int i = 40; switch (i) { case 20 * 2: ++i; } i; })");

    printf("All tests are passed!\n");

    return 0;
}
