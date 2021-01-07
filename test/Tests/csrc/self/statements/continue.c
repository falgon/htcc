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
        printf("[OK]:statements/continue test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:statements/continue test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int main()
{
    printf(">>>> tests: statements/continue\n");
            
    test_num = 1;
    assert(10, ({ int i = 0; int j = 0; for (; i < 10; ++i) { if (i > 5) continue; ++j; } i; }), "({ int i = 0; int j = 0; for (; i < 10; ++i) { if (i > 5) continue; ++j; } i; })");
    assert(6, ({ int i = 0; int j = 0; for (; i < 10; ++i) { if (i > 5) continue; ++j; } j; }), "({ int i = 0; int j = 0; for (; i < 10; ++i) { if (i > 5) continue; ++j; } j; })");
    assert(10, ({ int i = 0; int j = 0; for (; !i;) { for (; j != 10; ++j) continue; break; } j; }), "({ int i = 0; int j = 0; for (; !i;) { for (; j != 10; ++j) continue; break; } j; }),");
    assert(11, ({ int i = 0; int j = 0; while (i++ < 10) { if (i > 5) continue; ++j; } i; }), "({ int i = 0; int j = 0; while (i++ < 10) { if (i > 5) continue; ++j; } i; })");
    assert(5, ({ int i = 0; int j = 0; while (i++ < 10) { if (i > 5) continue; ++j; } j; }), "({ int i = 0; int j = 0; while (i++ < 10) { if (i > 5) continue; ++j; } j; })");
    assert(11, ({ int i = 0; int j = 0; while (!i) { while (j++ != 10) continue; break; } j; }), "({ int i = 0; int j = 0; while (!i) { while (j++ != 10) continue; break; } j; })");

    printf("All tests are passed!\n");
    return 0;
}
