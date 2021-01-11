// pointers test

int printf();
int exit();
int test_num;

int assert(long expected, long actual, char* code)
{
    if (expected == actual) {
        printf("[OK]:expressions/pointers test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:expressions/pointers test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int main()
{
    printf(">>>> tests: expressions/pointers\n");
    test_num = 1;
    assert(42, ({ int a = 42; int* b = &a; *b; }), "({ int a = 42; int* b = &a; *b; })");
    assert(42, ({ int a = 42; *&a; }), "({ int a = 42; *&a; })");
    assert(42, ({ int a = 42; int* b = &a; int** c = &b; **c; }), "({ int a = 42; int* b = &a; int** c = &b; **c; })");
    assert(84, ({ int a = 42; int* b = &a; *b = a * 2; a; }), "({ int a = 42; int* b = &a; *b = a * 2; a; })");
    assert(42, ({ int a = 42; int b = 5; *(&b+1); }), "({ int a = 42; int b = 5; *(&b+1); })");
    assert(53, ({ int a = 42; int b = 5; *(&a-1) = 53; b; }), "({ int a = 42; int b = 5; *(&a-1) = 53; b; })");
    assert(53, ({ int a = 42; int b = 5; *(&b+1) = 53; a; }), "({ int a = 42; int b = 5; *(&b+1) = 53; a; })");

    printf("All tests are passed!\n");
    return 0;
}
