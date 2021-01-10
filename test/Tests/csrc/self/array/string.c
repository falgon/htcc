// The array of string tests

int printf();
int exit();
int strcmp(char* p, char* q);
int test_num;

int assert(long expected, long actual, char* code)
{
    if (expected == actual) {
        printf("[OK]:array/string test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:array/string test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int main()
{
    printf(">>>> tests: array/string\n");
    test_num = 1;
    
    assert('h', ({ char s[5] = "hoge"; s[0]; }), "({ char s[5] = \"hoge\"; s[0]; })");
    assert('o', ({ char s[5] = "hoge"; s[1]; }), "({ char s[5] = \"hoge\"; s[1]; })");
    assert('g', ({ char s[5] = "hoge"; s[2]; }), "({ char s[5] = \"hoge\"; s[2]; })");
    assert('e', ({ char s[5] = "hoge"; s[3]; }), "({ char s[5] = \"hoge\"; s[3]; })");
    assert(0, ({ char s[5] = "hoge"; s[4]; }), "({ char s[5] = \"hoge\"; s[4]; })");
    assert('h', ({ char s[] = "hoge"; s[0]; }), "({ char s[] = \"hoge\"; s[0]; })");
    assert('o', ({ char s[] = "hoge"; s[1]; }), "({ char s[] = \"hoge\"; s[1]; })");
    assert('g', ({ char s[] = "hoge"; s[2]; }), "({ char s[] = \"hoge\"; s[2]; })");
    assert('e', ({ char s[] = "hoge"; s[3]; }), "({ char s[] = \"hoge\"; s[3]; })");
    assert(0, ({ char s[] = "hoge"; s[4]; }), "({ char s[] = \"hoge\"; s[4]; })");
    assert(0, ({ char s1[] = "hoge"; char s2[] = "hoge"; strcmp(s1, s2); }), "({ char s1[] = \"hoge\"; char s2[] = \"hoge\"; strcmp(s1, s2); })");
    assert(1, ({ char s1[] = "a"; char s2[] = "b"; 0 < strcmp(s1, s2); }), "({ char s1[] = \"a\"; char s2[] = \"b\"; 0 < strcmp(s1, s2); })");
    assert(5, ({ char s[] = "hoge"; sizeof s; }), "({ char s[] = \"hoge\"; sizeof s; })");
    assert('a', ({ char str[2][4] = { "abc", "def" }; str[0][0]; }), "({ char str[2][4] = { \"abc\", \"def\" }; str[0][0]; })");
    assert('b', ({ char str[2][4] = { "abc", "def" }; str[0][1]; }), "({ char str[2][4] = { \"abc\", \"def\" }; str[0][1]; })");
    assert('c', ({ char str[2][4] = { "abc", "def" }; str[0][2]; }), "({ char str[2][4] = { \"abc\", \"def\" }; str[0][2]; })");
    assert(0, ({ char str[2][4] = { "abc", "def" }; str[0][3]; }), "({ char str[2][4] = { \"abc\", \"def\" }; str[0][3]; })");
    assert('d', ({ char str[2][4] = { "abc", "def" }; str[1][0]; }), "({ char str[2][4] = { \"abc\", \"def\" }; str[1][0]; })");
    assert('e', ({ char str[2][4] = { "abc", "def" }; str[1][1]; }), "({ char str[2][4] = { \"abc\", \"def\" }; str[1][1]; })");
    assert('f', ({ char str[2][4] = { "abc", "def" }; str[1][2]; }), "({ char str[2][4] = { \"abc\", \"def\" }; str[1][2]; })");
    assert(0, ({ char str[2][4] = { "abc", "def" }; str[1][3]; }), "({ char str[2][4] = { \"abc\", \"def\" }; str[1][3]; })");
    assert('a', ({ char str[][4] = { "abc", "def" }; str[0][0]; }), "({ char str[][4] = { \"abc\", \"def\" }; str[0][0]; })");
    assert('b', ({ char str[][4] = { "abc", "def" }; str[0][1]; }), "({ char str[][4] = { \"abc\", \"def\" }; str[0][1]; })");
    assert('c', ({ char str[][4] = { "abc", "def" }; str[0][2]; }), "({ char str[][4] = { \"abc\", \"def\" }; str[0][2]; })");
    assert(0, ({ char str[][4] = { "abc", "def" }; str[0][3]; }), "({ char str[][4] = { \"abc\", \"def\" }; str[0][3]; })");
    assert('d', ({ char str[][4] = { "abc", "def" }; str[1][0]; }), "({ char str[][4] = { \"abc\", \"def\" }; str[1][0]; })");
    assert('e', ({ char str[][4] = { "abc", "def" }; str[1][1]; }), "({ char str[][4] = { \"abc\", \"def\" }; str[1][1]; })");
    assert('f', ({ char str[][4] = { "abc", "def" }; str[1][2]; }), "({ char str[][4] = { \"abc\", \"def\" }; str[1][2]; })");
    assert(0, ({ char str[][4] = { "abc", "def" }; str[1][3]; }), "({ char str[][4] = { \"abc\", \"def\" }; str[1][3]; })");

    printf("All tests are passed!\n");

    return 0;
}
