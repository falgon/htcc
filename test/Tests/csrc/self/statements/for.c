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
        printf("[OK]:statements/for test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:statements/for test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int main()
{
    printf(">>>> tests: statements/for\n");
            
    test_num = 1;
    assert(110, ({ int a = 0; int i = 0; for (i = 1; i <= 10; i = i + 1) a = a + i * 2; a; }), "({ int a = 0; int i = 0; for (i = 1; i <= 10; i = i + 1) a = a + i * 2; a; })");
    assert(12, ({ int i = 0; for (; i <= 10;) i = i + 2; i; }), "({ int i = 0; for (; i <= 10;) i = i + 2; i; })");
    assert(0, ({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) if (a) a = 0; else a = 1; a; }), "({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) if (a) a = 0; else a = 1; a; })");
    assert(0, ({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) { a = a + i; a = a - i; } a; }), "({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) { a = a + i; a = a - i; } a; })");
    assert(20, ({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) if (i % 2 == 0) a = a + i; a; }), "({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) if (i % 2 == 0) a = a + i; a; })");
    assert(6, ({ int s = 0; int i = 1; for (; i < 4; i = i + 1) s = s + i; s; }), "({ int s = 0; int i = 1; for (; i < 4; i = i + 1) s = s + i; return s; })");
    assert(3, ({ int a = 0; for(; a < 3; a = a + 1); a; }), "({ int a = 0; for(; a < 3; a = a + 1); a; })");
    assert(45, ({ int ar[10]; int i = 0; for (; i < 10; i = i + 1) { *(ar + i) = i; } int s = 0; for (i = 0; i < 10; i = i + 1) { s = s + *(ar + i); } s; }), "({ int ar[10]; int i = 0; for (; i < 10; i = i + 1) { *(ar + i) = i; } int s = 0; for (i = 0; i < 10; i = i + 1) { s = s + *(ar + i); } s; })");
    assert(9, ({ int ar[2][3]; int s = 0; int i = 0; for (; i < 2; i = i + 1) { int j = 0; for (; j < 3; j = j + 1) { *(*(ar + i) + j) = i + j; s = s + *(*(ar + i) + j); } } s; }), "({ int ar[2][3]; int s = 0; int i = 0; for (; i < 2; i = i + 1) { int j = 0; for (; j < 3; j = j + 1) { *(*(ar + i) + j) = i + j; s = s + *(*(ar + i) + j); } } s; })");
    assert(0, ({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[0]; }), "({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[0]; })");
    assert(1, ({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[1]; }), "({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[1]; })");
    assert(2, ({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[2]; }), "({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[2]; })");
    assert(42, ({ int i = 42; for (int i = 0; i < 10; ++i); i; }), "({ int i = 42; for (int i = 0; i < 10; ++i); i; })");
    assert(42, ({ int i = 42; for (auto int i = ({ int i = 0; for (; i < 10; ++i); i; }); i > 0; --i); i; }), "for (int i = ({ int i = 0; for (; i < 10; ++i); i; }); i > 0; --i); i; })");
    
    printf("All tests are passed!\n");

    return 0;
}
