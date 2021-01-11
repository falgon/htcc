// This is a c compiler test file. This comment itself is a line comment test.
/*
 * This comment is also a block comment test.
 */

int printf();
int exit();
int strcmp(char* p, char* q);
int test_num;

int gr[3];
//int (*gpa)[3];

int assert(long expected, long actual, char* code)
{
    if (expected == actual) {
        printf("[OK]:array/basic test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]:array/basic test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int gg(int* p) { *p = 42; return 0; } 
int sum(int* p, int n) { int s = 0; for (int i = 0; i < n; i = i + 1) s = s + *(p + i); return s; }

int main()
{
    printf(">>>> tests: array/basic\n");

    test_num = 1;
    assert(3, ({ int ar[2]; int* p = ar; *p = 3; *ar; }), "({ int ar[2]; int* p = ar; *p = 3; *ar; })");
    assert(3, ({ int ar[2]; int* p = ar; *(p + 1) = 3; *(ar + 1); }), "({ int ar[2]; int* p = ar; *(p + 1) = 3; *(ar + 1); })");
    assert(5, ({ int ar[2]; int* p = ar; *p = 2; *(p + 1) = 3; *ar + *(ar + 1); }), "({ int ar[2]; int* p = ar; *p = 2; *(p + 1) = 3; *ar + *(ar + 1); })");
    assert(1, ({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *ar; }), "({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *ar; })");
    assert(2, ({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *(ar + 1); }), "({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *(ar + 1); })");
    assert(3, ({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *(ar + 2); }), "({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *(ar + 2); })");
    assert(42, ({ int a = 0; gg(&a); a; }), "({ int a = 0; gg(&a); a; })");
    assert(45, ({ int ar[10]; int i = 0; for (; i < 10; i = i + 1) { *(ar + i) = i; } int s = 0; for (i = 0; i < 10; i = i + 1) { s = s + *(ar + i); } s; }), "({ int ar[10]; int i = 0; for (; i < 10; i = i + 1) { *(ar + i) = i; } int s = 0; for (i = 0; i < 10; i = i + 1) { s = s + *(ar + i); } s; })");
    assert(45, ({ int ar[10]; int i = 0; for (; i < 10; i = i + 1) *(ar + i) = i; sum(ar, 10); }), "({ int ar[10]; int i = 0; for (; i < 10; i = i + 1) *(ar + i) = i; sum(ar, 10); })");
    assert(9, ({ int ar[2][3]; int s = 0; int i = 0; for (; i < 2; i = i + 1) { int j = 0; for (; j < 3; j = j + 1) { *(*(ar + i) + j) = i + j; s = s + *(*(ar + i) + j); } } s; }), "({ int ar[2][3]; int s = 0; int i = 0; for (; i < 2; i = i + 1) { int j = 0; for (; j < 3; j = j + 1) { *(*(ar + i) + j) = i + j; s = s + *(*(ar + i) + j); } } s; })");
    assert(42, ({ int ar[2][3]; int* p = ar; *p = 42; **ar; }), "({ int ar[2][3]; int* p = ar; *p = 42; **ar; }");
    assert(42, ({ int ar[2][3]; int* p = ar; *(p + 1) = 42; *(*ar + 1); }), "({ int ar[2][3]; int* p = ar; *(p + 1) = 42; *(*ar + 1); })");
    assert(42, ({ int ar[2][3]; int* p = ar; *(p + 2) = 42; *(*ar + 2); }), "({ int ar[2][3]; int* p = ar; *(p + 2) = 42; *(*ar + 2); })");
    assert(42, ({ int ar[2][3]; int* p = ar; *(p + 3) = 42; **(ar + 1); }), "({ int ar[2][3]; int* p = ar; *(p + 3) = 42; **(ar + 1); })");
    assert(42, ({ int ar[2][3]; int* p = ar; *(p + 4) = 42; *(*(ar + 1) + 1); }), "({ int ar[2][3]; int* p = ar; *(p + 4) = 42; *(*(ar + 1) + 1); })");
    assert(42, ({ int ar[2][3]; int* p = ar; *(p + 5) = 42; *(*(ar + 1) + 2); }), "({ int ar[2][3]; int* p = ar; *(p + 5) = 42; *(*(ar + 1) + 2); })");
    assert(42, ({ int ar[2][3]; int* p = ar; *(p + 6) = 42; **(ar + 2); }), "({ int ar[2][3]; int* p = ar; *(p + 6) = 42; **(ar + 2); })");
    assert(0, ({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[0]; }), "({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[0]; })");
    assert(1, ({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[1]; }), "({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[1]; })");
    assert(2, ({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[2]; }), "({ int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; ar[2]; })");
    assert(42, ({ int ar[2][3]; int* p = ar; p[0] = 42; ar[0][0]; }), "({ int ar[2][3]; int* p = ar; p[0] = 42; ar[0][0]; })");
    assert(42, ({ int ar[2][3]; int* p = ar; p[1] = 42; ar[0][1]; }), "({ int ar[2][3]; int* p = ar; p[1] = 42; ar[0][1]; })");
    assert(42, ({ int ar[2][3]; int* p = ar; p[2] = 42; ar[0][2]; }), "({ int ar[2][3]; int* p = ar; p[2] = 42; ar[0][2]; })");
    assert(42, ({ int ar[2][3]; int* p = ar; p[3] = 42; ar[1][0]; }), "({ int ar[2][3]; int* p = ar; p[3] = 42; ar[1][0]; })");
    assert(42, ({ int ar[2][3]; int* p = ar; p[4] = 42; ar[1][1]; }), "({ int ar[2][3]; int* p = ar; p[4] = 42; ar[1][1]; })");
    assert(42, ({ int ar[2][3]; int* p = ar; p[5] = 42; ar[1][2]; }), "({ int ar[2][3]; int* p = ar; p[5] = 42; ar[1][2]; })");
    assert(42, ({ int ar[2][3]; int* p = ar; p[6] = 42; ar[2][0]; }), "({ int ar[2][3]; int* p = ar; p[6] = 42; ar[2][0]; })");
    assert(3 * 4, ({ int ar[3]; sizeof ar; }), "({ int ar[3]; sizeof ar; })");
    assert(3 * 5 * 4, ({int ar[3][5]; sizeof ar; }), "({int ar[3][5]; return sizeof ar; })");
    assert(5 * 4, ({ int ar[3][5]; sizeof *ar; }), "({ int ar[3][5]; sizeof *ar; })");
    assert(4, ({ int ar[3][5]; sizeof **ar; }), "({ int ar[3][5]; sizeof **ar; })");
    assert(4 + 1, ({ int ar[3][5]; sizeof(**ar) + 1; }), "({ int ar[3][5]; sizeof(**ar) + 1; })");
    assert(4 + 1, ({ int ar[3][5]; sizeof **ar + 1; }), "({ int ar[3][5]; return sizeof **ar + 1; })");
    assert(8, ({ int ar[3][5]; sizeof(**ar + 1); }), "({ int ar[3][5]; sizeof(**ar + 1); })");
    assert(42, ({ int ar[2]; 2[ar] = 42; ar[2]; }), "({ int ar[2]; 2[ar] = 42; ar[2]; })");
    assert(1, ({ int i = 0; for (; i < sizeof gr / sizeof gr[0]; i = i + 1) gr[i] = i + 1; gr[0]; }), "({ int i = 0; for (; i < sizeof gr / sizeof gr[0]; i = i + 1) gr[i] = i + 1; gr[0]; })");
    assert(2, gr[1], "gr[1]");
    assert(3, gr[2], "gr[2]");
    assert(10, ({ char ar[10]; sizeof ar; }), "({ char ar[10]; return sizeof ar; })");
    assert(97, "abc"[0], "\"abc\"[0]");
    assert(98, "abc"[1], "\"abc\"[1];");
    assert(99, "abc"[2], "\"abc\"[2];");
    assert(0, "abc"[3], "\"abc\"[3];");
    assert(99, ({ char* p = "abc"; p[2]; }), "({ char* p = \"abc\"; p[2]; })");
    assert(7, "\a"[0], "\"\\a\"[0]");
    assert(8, "\b"[0], "\"\\b\"[0]");
    assert(9, "\t"[0], "\"\\t\"[0]");
    assert(10, "\n"[0], "\"\\n\"[0]");
    assert(11, "\v"[0], "\"\\v\"[0]");
    assert(12, "\f"[0], "\"\\f\"[0]");
    assert(13, "\r"[0], "\"\\r\"[0]");
    assert(0, "\0"[0], "\\0[0]");
    assert(92, "\\"[0], "a");
    assert(2, ({ int ar[5]; int* p = ar + 2; p - ar; }), "({ int ar[5]; int* p = ar + 2; p - ar; })");
    assert(1, ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; *p++; }), " ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; *p++; })");
    assert(2, ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; ++*p++; }), " ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; *p++; })");
    assert(1, ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; *p--; }), " ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; *p++; })");
    assert(0, ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; --*p; }), " ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; --*p; })");
    assert(0, ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; *p++; ar[0]; }), " ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; *p++; ar[0]; })");
    assert(0, ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; (*p++)--; ar[1]; }), " ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; (*p++)--; ar[1]; })");
    assert(2, ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; (*p++)--; ar[2]; }), " ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; (*p++)--; ar[2]; })");
    assert(2, ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; (*p++)--; *p; }), " ({ int ar[3]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; (*p++)--; *p; })");
    assert(1, ({ int ar[2]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar; p += 1; *p; }), "({ int ar[2]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar; p += 1; *p; })");
    assert(0, ({ int ar[2]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; p -= 1; *p; }), "({ int ar[2]; int i = 0; for (; i < sizeof ar / sizeof *ar; ++i) ar[i] = i; int* p = ar + 1; p -= 1; *p; })");
    assert(10, ({ int ar[2 ? 5 * 2 : 5]; sizeof ar / sizeof *ar; }), "({ int ar[2 ? 5 * 2 : 5]; sizeof ar / sizeof *ar; })");
    assert(0, ({ int ar[]; 0; }), "({ int ar[]; 0; })");
    assert(42, ({ int ar[1] = { 42 }; ar[0]; }), "({ int ar[1] = { 42 }; ar[0]; })");
    assert(1, ({ int ar[3] = { 1, 2, 3 }; ar[0]; }), "({ int ar[3] = { 1, 2, 3 }; ar[0]; })");
    assert(2, ({ int ar[3] = { 1, 2, 3 }; ar[1]; }), "({ int ar[3] = { 1, 2, 3 }; ar[1]; })");
    assert(3, ({ int ar[3] = { 1, 2, 3 }; ar[2]; }), "({ int ar[3] = { 1, 2, 3 }; ar[2]; })");
    assert(2, ({ int ar[2][3] = { { 1, 2, 3 }, { 4, 5, 6 }}; ar[0][1]; }), "({ int ar[2][3] = { { 1, 2, 3 }, { 4, 5, 6 }}; ar[0][1]; })");
    assert(4, ({ int ar[2][3] = { { 1, 2, 3 }, { 4, 5, 6 }}; ar[1][0]; }), "({ int ar[2][3] = { { 1, 2, 3 }, { 4, 5, 6 }}; ar[1][0]; })");
    assert(6, ({ int ar[2][3] = { { 1, 2, 3 }, { 4, 5, 6 }}; ar[1][2]; }), "({ int ar[2][3] = { { 1, 2, 3 }, { 4, 5, 6 }}; ar[1][2]; })");
    assert(1, ({ int a = 0; int ar[2] = { a = 1 }; ar[0]; }), "({ int a = 0; int ar[2] = { a = 1 }; ar[0]; })");
    assert(1, ({ int a = 0; int ar[2] = { a = 1 }; a; }), "({ int a = 0; int ar[2] = { a = 1 }; a; })");
    assert(0, ({ int ar[3] = {}; ar[0]; }), "({ int ar[3] = {}; ar[0]; })");
    assert(0, ({ int ar[3] = {}; ar[1]; }), "({ int ar[3] = {}; ar[1]; })");
    assert(0, ({ int ar[3] = {}; ar[2]; }), "({ int ar[3] = {}; ar[2]; })");
    assert(0, ({ int ar[3][2] = {}; ar[0][0]; }), "({ int ar[3][2] = {}; ar[0][0]; })");
    assert(0, ({ int ar[3][2] = {}; ar[0][1]; }), "({ int ar[3][2] = {}; ar[0][1]; })");
    assert(0, ({ int ar[3][2] = {}; ar[1][0]; }), "({ int ar[3][2] = {}; ar[1][0]; })");
    assert(0, ({ int ar[3][2] = {}; ar[1][1]; }), "({ int ar[3][2] = {}; ar[1][1]; })");
    assert(0, ({ int ar[3][2] = {}; ar[2][0]; }), "({ int ar[3][2] = {}; ar[2][0]; })");
    assert(0, ({ int ar[3][2] = {}; ar[2][1]; }), "({ int ar[3][2] = {}; ar[2][1]; })");
    assert(2, ({ int ar[2][3] = { { 42, 2 } }; ar[0][1]; }), "({ int ar[2][3] = { { 42, 2 } }; ar[0][1]; })");
    assert(0, ({ int ar[2][3] = { { 42, 2 } }; ar[1][0]; }), "({ int ar[2][3] = { { 42, 2 } }; ar[1][0]; })");
    assert(0, ({ int ar[2][3] = { { 42, 2 } }; ar[1][2]; }), "({ int ar[2][3] = { { 42, 2 } }; ar[1][2]; })");
    assert(0, ({ int ar[3][2] = { {}, {}, {} }; ar[0][0]; }), "({ int ar[3][2] = { {}, {}, {} }; ar[0][0]; })");
    assert(0, ({ int ar[3][2] = { {}, {}, {} }; ar[0][1]; }), "({ int ar[3][2] = { {}, {}, {} }; ar[0][1]; })");
    assert(0, ({ int ar[3][2] = { {}, {}, {} }; ar[1][0]; }), "({ int ar[3][2] = { {}, {}, {} }; ar[1][0]; })");
    assert(0, ({ int ar[3][2] = { {}, {}, {} }; ar[1][1]; }), "({ int ar[3][2] = { {}, {}, {} }; ar[1][1]; })");
    assert(0, ({ int ar[3][2] = { {}, {}, {} }; ar[2][0]; }), "({ int ar[3][2] = { {}, {}, {} }; ar[2][0]; })");
    assert(0, ({ int ar[3][2] = { {}, {}, {} }; ar[2][1]; }), "({ int ar[3][2] = { {}, {}, {} }; ar[2][1]; })");
    assert(0, ({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[0][0]; }), "({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[0][0]; })");
    assert(0, ({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[0][1]; }), "({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[0][1]; })");
    assert(0, ({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[1][0]; }), "({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[1][0]; })");
    assert(0, ({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[1][1]; }), "({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[1][1]; })");
    assert(1, ({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[2][0]; }), "({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[2][0]; })");
    assert(2, ({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[2][1]; }), "({ int ar[3][2] = { {}, {}, { 1, 2 } }; ar[2][1]; })");
    assert(1, ({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[0][0]; }), "({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[0][0]; })");
    assert(2, ({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[0][1]; }), "({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[0][1]; })");
    assert(3, ({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[0][2]; }), "({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[0][2]; })");
    assert(0, ({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[1][0]; }), "({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[1][0]; })");
    assert(0, ({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[1][1]; }), "({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[1][1]; })");
    assert(0, ({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[1][2]; }), "({ int ar[2][3] = { { 1, 2, 3 }, {} }; ar[1][2]; })");
    assert(0, ({ int ar[1][1][1] = {{{}}}; ar[0][0][0]; }), "({ int ar[1][1][1] = {{{}}}; ar[0][0][0]; })");
    
    printf("All tests are passed!\n");

    return 0;
}
