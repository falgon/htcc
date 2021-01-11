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
        printf("[OK]:statements/goto test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:statements/goto test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int main()
{
    printf(">>>> tests: statements/goto\n");
    test_num = 1;
    
    assert(3, ({ int i = 0; goto a; a: ++i; b: ++i; c: ++i; i; }), "({ int i = 0; goto a; a: ++i; b: ++i; c: ++i; i; })");
    assert(2, ({ int i = 0; goto e; d: ++i; e: ++i; f: ++i; i; }), "({ int i = 0; goto e; d: ++i; e: ++i; f: ++i; i; })");
    assert(1, ({ int i = 0; goto i; g: ++i; h: ++i; i: ++i; i; }), "({ int i = 0; goto i; g: ++i; h: ++i; i: ++i; i; })");

    printf("All tests are passed!\n");

    return 0;
}
