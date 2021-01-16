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
        printf("[OK]:expressions/cast test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:expressions/cast test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int main()
{
    printf(">>>> tests: expressions/cast\n");
    test_num = 1;

    assert(4, sizeof((int)'a'));
    assert(1, sizeof((char)42));
    assert(8, sizeof((int*)42));
    assert(8, sizeof((char*)42));

    printf("All tests are passed!\n");
    return 0;
}
