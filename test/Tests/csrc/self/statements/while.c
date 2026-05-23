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
        printf("[OK]:statements/while test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:statements/while test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int main()
{
    printf(">>>> tests: statements/while\n");

    test_num = 1;
    assert(10, ({ int a = 1; while (a < 10) a = a + 1; a; }), "({{ int a = 1; while (a < 10) a = a + 1; a; })");
    assert(31, ({ int a = 1; while (a < 10) a = a + 1; int b = 1; while (b < 20) b = b + 2; a + b; }), "({ int a = 1; while (a < 10) a = a + 1; int b = 1; while (b < 20) b = b + 2; a + b; })");
    assert(0, ({ int a = 0; while (a); 0; }), "({ int a = 0; while (a); 0; })");
    assert(4, ({ int i = 0; while (1) { if (i++ == 3) break; } i; }), "({ int i = 0; while (1) { if (i++ == 3) break; } i; })");
    assert(4, ({ int i = 0; while (1) { for (;;) break; if (i++ == 3) break; } i; }), "({ int i = 0; while (1) { for (;;) break; if (i++ == 3) break; } i; })");
    
    printf("All tests are passed!\n");

    return 0;
}
