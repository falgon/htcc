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
        printf("[OK]:expressions/operators test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:expressions/operators test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int sub3(int a, int b, int c) { return a - b - c; }

int main()
{
    printf(">>>> tests: expressions/operators\n");
    test_num = 1;
    assert(42, 42, "42");
    assert(7, 1 + 2 + 4, "1 + 2 + 4");
    assert(6, 10 - 7 + 3, "10 - 7 + 3");
    assert(35, 42 + 23 - 30, "42 + 23 - 30");
    assert(18, 42 / 2 + 2 - 5, "42 / 2 + 2 - 5");
    assert(4, (3 + 5) / 2, "(3 + 5) / 2");
    assert(21, (4 - 2) * 8 + 20 / 4, "(4 - 2) * 8 + 20 / 4");
    assert(15, -(-3 * +5), "-(-3 * +5)");
    assert(5, -25 + 30, "-25 + 30");
    assert(1, 42 == 42, "42 == 42");
    assert(1, 42 != 53, "42 != 53");
    assert(1, 42 < 53, "42 < 53");
    assert(1, 53 > 42, "53 > 42");
    assert(1, 42 <= 42, "42 <= 42");
    assert(1, 32 <= 42, "32 <= 42");
    assert(1, 42 >= 42, "42 >= 42");
    assert(1, 53 >= 42, "53 >= 42");
    assert(1, (1 + 1) == 2, "(1 + 1) == 2");
    assert(1, (2 * 3) != 2, "(2 * 3) != 2");
    assert(1, ({ int a = 1; int b = 1; a & b; }), "({ int a = 1; int b = 1; return a & b; })");
    assert(1, ({ int a = 42; int b = 53; a = a ^ b; b = b ^ a; a = a ^ b; if (a == 53) if (b == 42) a = 1; else a = 0; a; }), "({ int a = 42; int b = 53; a = a ^ b; b = b ^ a; a = a ^ b; if (a == 53) if (b == 42) a = 1; else a = 0; a; })");
    assert(1, 1 | 0, "1 | 0");
    assert(1, ({ int a = 1; int b = 0; a & b ^ a | b; }), "({ int a = 1; int b = 0; a & b ^ a | b; })"); // Xor swap
    assert(20, ({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) if (i % 2 == 0) a = a + i; a; }), "({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) if (i % 2 == 0) a = a + i; a; })");
    assert(1, !0, "!0");
    assert(0, !42, "!42");
    assert(1, !!!0, "!!!0");
    assert(41, ~(-42), "~(-42)");
    assert(42, ~~~~42, "~~~~42");
    assert(1, (2 * 4) == (2 << 2), "(2 * 4) == (2 << 2)");
    assert(1, (8 / 4) == (8 >> 2), "(8 / 4) == (8 >> 2)");
    assert(1, ({ int a = 2 << 4; (a & (a - 1)) == 0; }), "({ int a = 2 << 4; (a & (a - 1)) == 0; })"); // Determining if an integer is a power of 2
    assert(3, ({ 1; {2;} 3; }), "({ 1; {2;} 3; })");
    assert(4, ({ int a; sizeof(a); }), "({ int a; sizeof(a); })");
    assert(4, ({ int a; sizeof a; }), "({ int a; sizeof a; })");
    assert(8, ({ int* p; sizeof p; }), "({ int* p; sizeof p; })");
    assert(42, ({ int a = 41; ++a; }), "({ int a = 41; ++a; })");
    assert(42, ({ int a = 43; --a; }), "({ int a = 43; --a; })");
    assert(42, ({ int a = 42; a++; }), "({ int a = 41; a++; })");
    assert(42, ({ int a = 42; a--; }), "({ int a = 43; a--; })");
    assert(42, ({ int a = 41; a++; a; }), "({ int a = 41; a++; a; })");
    assert(42, ({ int a = 43; a--; a; }), "({ int a = 43; a--; a; })");
    assert(42, ({ int a = 2; a += 40; a; }), "int a = 2; a += 40; a;");
    assert(42, ({ int a = 2; a += 40; }), "int a = 2; a += 40;");
    assert(42, ({ int a = 44; a -= 2; a; }), "int a = 44; a -= 2; a;");
    assert(42, ({ int a = 44; a -= 2; }), "int a = 44; a -= 2;");
    assert(42, ({ int a = 21; a *= 2; a; }), "int a = 21; a *= 2; a;");
    assert(42, ({ int a = 21; a *= 2; }), "int a = 21; a *= 2;");
    assert(42, ({ int a = 84; a /= 2; a; }), "int a = 84; a /= 2; a;");
    assert(42, ({ int a = 84; a /= 2; }), "int a = 84; a /= 2;");
    assert(1, 1 || 0, "1 || 0");
    assert(1, (1 + 1) || 0 || 0, "(1 + 1) || 0 || 0");
    assert(0, 0 || 0, "0 || 0");
    assert(0, 0 || (1 - 1), "0 || (1 - 1)");
    assert(1, 1 && 2, "1 && 2");
    assert(0, 2 && 3 && 4 && 0, "2 && 3 && 4 && 0");
    assert(2, ({ int a = 6; a &= 3; a; }), "({ int a = 6; a &= 3; a; })");
    assert(2, ({ int a = 6; a &= 3; }), "({ int a = 6; a &= 3; })");
    assert(7, ({ int a = 6; a |= 3; a; }), "({ int a = 6; a |= 3; a; })");
    assert(7, ({ int a = 6; a |= 3; }), "({ int a = 6; a |= 3; })");
    assert(10, ({ int a = 15; a ^= 5; a; }), "({ int a = 15; a ^= 5; a; })");
    assert(10, ({ int a = 15; a ^= 5; }), "({ int a = 15; a ^= 5; })");
    assert(2, ({ int a = 1; a <<= 1; a; }), "({ int a = 1; a <<= 1; a; })");
    assert(2, ({ int a = 1; a <<= 1; }), "({ int a = 1; a <<= 1; })");
    assert(2, ({ int a = 4; a >>= 1; a; }), "({ int a = 4; a >>= 1; a; })");
    assert(2, ({ int a = 4; a >>= 1; }), "({ int a = 4; a >>= 1; })");
    assert(-1, ({ int a = -1; a >>= 1; }), "({ int a = -1; a >>= 1; })");
    assert(42, 1 ? 42 : 0, "1 ? 42 : 0");
    assert(42, 0 ? 0 : 42, "0 ? 0 : 42");
    assert(42, ({ int a = 1; int b = 0; a || b ? 42 : 0; }), "({ int a = 1; int b = 0; a || b ? 42 : 0; })");
    assert(42, ({ int a = 1; int b = 0; a && b ? 0 : 42; }), "({ int a = 1; int b = 0; a && b ? 0 : 42; })");
    assert(42, ({ 42 ?: 0; }), "({ 42 ?: 0; })");
    assert(42, ({ int a = 42; a++ ?: 0; }),  "({ int a = 42; a++ ?: 0; })");
    assert(42, ({ sub3(2, 1, 1) ?: 42; }), "({ sub3(2, 1, 1) ?: 42; })");
    assert(4, ({ int a; sizeof(a); }), "({ int a; sizeof(a); })");
    assert(4, ({ int a; sizeof a; }), "({ int a; sizeof a; })");
    assert(8, ({ int* p; sizeof p; }), "({ int* p; sizeof p; })");
    
    printf("All tests are passed!\n");

    return 0;
}
