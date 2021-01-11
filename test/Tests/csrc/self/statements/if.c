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
        printf("[OK]:statements/if test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:statements/if test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int main()
{
    printf(">>>> tests: statements/if\n");
    test_num = 1;

    assert(42, ({ int a = 0; if (1) { a = 42; } else { a = 53; } a; }), "({ int a = 0; if (1) { a = 42; } else { a = 53; } a; })");
    assert(53, ({ int a = 0; if (20*3-60) a = 42; else a = 53; a; }), "({ int a = 0; if (20*3-60) a = 42; else a = 53; a; })");
    assert(2, ({ int a = 1; int b = 2; if (a) a = b; else a = 42; a; }), "({ int a = 1; int b = 2; if (a) a = b; else a = 42; a; })");
    assert(53, ({ int a = 0; if (0) a = 42; else a = 53; a; }), "({ int a = 0; if (0) a = 42; else a = 53; a; })");
    assert(4, ({ int a = 0; int b = 2; if (a) a = b; else a = b * 2; a; }), "({ int a = 0; int b = 2; if (a) a = b; else a = b * 2; a; })");
    assert(1, ({ int a = 1; int b = 0; if (b) a = 42; if (0) a = 42; a; }), "({ int a = 1; int b = 0; if (b) a = 42; if (0) a = 42; a; })");
    assert(2, ({ int a = 1; int b = 2; if (a) if (b) a = b; else a = 53; else a = 24; a; }), "({ int a = 1; int b = 2; if (a) if (b) a = b; else a = 53; else a = 24; a; })");
    assert(2, ({ int a = 0; if (1) if (1) if (1) if (1) if (1) if (0) a = 1; else a = 2; else a = 3; else a = 4; else a = 5; else a = 6; else a = 7; a; }), "({ int a = 0; if (1) if (1) if (1) if (1) if (1) if (0) a = 1; else a = 2; else a = 3; else a = 4; else a = 5; else a = 6; else a = 7; a; })");
    assert(42, ({ int a = 0; if(1) if(1) a = 42; else a = 53; a; }), "({ int a = 0; if(1) if(1) a = 42; else a = 53; a; })");
    assert(0, ({ if(0); 0; }), "({ if(0); 0; })");

    printf("All tests are passed!\n");

    return 0;
}
