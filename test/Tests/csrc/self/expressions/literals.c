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
        printf("[OK]:expressions/literals test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:expressions/literals test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int main()
{
    printf(">>>> tests: expressions/literals\n");
    test_num = 1;
    assert(511, 0777, "0777");
    assert(0, 0x0, "0x0");
    assert(10, 0xa, "0xa");
    assert(10, 0Xa, "0Xa");
    assert(48879, 0xbeef, "0xbeef");
    assert(48879, 0xBEEF, "0xBEEF");
    /*assert(0, 0b0, "0b0");
    assert(1, 0b1, "0b1");
    assert(42, 0b101010, "0b101010");
    assert(42, 0B101010, "0B101010");*/
    assert(49389, 0xc0ed, "0xc0ed");
    assert(49389, 0xC0eD, "0xC0eD");
    assert(97, 'a', "'a'");
    assert(10, '\n', "\'\\n\'");

    printf("All tests are passed!\n");

    return 0;
}
