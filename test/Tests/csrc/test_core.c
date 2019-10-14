// This is a c compiler test file. This comment itself is a line comment test.
/*
 * This comment is also a block comment test.
 */

int test_num;
int g;
int gr[3];

int assert(int expected, int actual, char* code)
{

    if (expected == actual) {
        printf("[OK]: test #%d: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]: test #%d: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int f() { return 42; }
int add(int x, int y) { return x + y; }
int rec(int a) { if (a == 0) return 42; return rec(a - 1); } 
int fib(int n) { if (n == 0) return 1; else if (n == 1) return 1; else if (n >= 2) return fib(n - 1) + fib(n - 2); else return 0; }
int gg(int* p) { *p = 42; return 0; } 
int sum(int* p, int n) { int sum = 0; int i = 0; for (; i < n; i = i + 1) sum = sum + *(p + i); return sum; }
int sub3(int a, int b, int c) { return a - b - c; }

int main()
{
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
    assert(62, ({ int a = 42; int b = 20; a + b; }), "({ int a = 42; int b = 20; a + b; })");
    assert(20, ({ int a = 42; int b = 20; int c = 32; (a - c) * b / 10; }), "({ int a = 42; int b = 20; int c = 32; (a - c) * b / 10; })");
    assert(22, ({ int hoge = 42; int foo = 20; hoge - foo; }), "({ int hoge = 42; int foo = 20; hoge - foo; })");
    assert(14, ({ int a = 3; int b = 5 * 6 - 8; a + b / 2; }), "({ int a = 3; int b = 5 * 6 - 8; a + b / 2; })");
    assert(42, ({ int a = 0; if (1) { a = 42; } else { a = 53; } a; }), "({ int a = 0; if (1) { a = 42; } else { a = 53; } a; })");
    assert(53, ({ int a = 0; if (20*3-60) a = 42; else a = 53; a; }), "({ int a = 0; if (20*3-60) a = 42; else a = 53; a; })");
    assert(2, ({ int a = 1; int b = 2; if (a) a = b; else a = 42; a; }), "({ int a = 1; int b = 2; if (a) a = b; else a = 42; a; })");
    assert(53, ({ int a = 0; if (0) a = 42; else a = 53; a; }), "({ int a = 0; if (0) a = 42; else a = 53; a; })");
    assert(4, ({ int a = 0; int b = 2; if (a) a = b; else a = b * 2; a; }), "({ int a = 0; int b = 2; if (a) a = b; else a = b * 2; a; })");
    assert(1, ({ int a = 1; int b = 0; if (b) a = 42; if (0) a = 42; a; }), "({ int a = 1; int b = 0; if (b) a = 42; if (0) a = 42; a; })");
    assert(2, ({ int a = 1; int b = 2; if (a) if (b) a = b; else a = 53; else a = 24; a; }), "({ int a = 1; int b = 2; if (a) if (b) a = b; else a = 53; else a = 24; a; })");
    assert(2, ({ int a = 0; if (1) if (1) if (1) if (1) if (1) if (0) a = 1; else a = 2; else a = 3; else a = 4; else a = 5; else a = 6; else a = 7; a; }), "({ int a = 0; if (1) if (1) if (1) if (1) if (1) if (0) a = 1; else a = 2; else a = 3; else a = 4; else a = 5; else a = 6; else a = 7; a; })");
    assert(42, ({ int a = 0; if(1) if(1) a = 42; else a = 53; a; }), "({ int a = 0; if(1) if(1) a = 42; else a = 53; a; })");
    assert(0, ({ if(0); 0; }), "({ if(0); 0; })");
    assert(10, ({ int a = 1; while (a < 10) a = a + 1; a; }), "({{ int a = 1; while (a < 10) a = a + 1; a; })");
    assert(31, ({ int a = 1; while (a < 10) a = a + 1; int b = 1; while (b < 20) b = b + 2; a + b; }), "({ int a = 1; while (a < 10) a = a + 1; int b = 1; while (b < 20) b = b + 2; a + b; })");
    assert(0, ({ int a = 0; while (a); 0; }), "({ int a = 0; while (a); 0; })");
    assert(110, ({ int a = 0; int i = 0; for (i = 1; i <= 10; i = i + 1) a = a + i * 2; a; }), "({ int a = 0; int i = 0; for (i = 1; i <= 10; i = i + 1) a = a + i * 2; a; })");
    assert(12, ({ int i = 0; for (; i <= 10;) i = i + 2; i; }), "({ int i = 0; for (; i <= 10;) i = i + 2; i; })");
    assert(0, ({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) if (a) a = 0; else a = 1; a; }), "({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) if (a) a = 0; else a = 1; a; })");
    assert(0, ({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) { a = a + i; a = a - i; } a; }), "({ int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) { a = a + i; a = a - i; } a; })");
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
    assert(42, f(), "f()");
    assert(45, f() + 3,  "f() + 3");
    assert(3, add(1, 2), "add(1, 2)");
    assert(44, ({ int b = rec(2); b + 2; }), "({ int b = rec(2); b + 2; })");
    assert(8, fib(5), "fib(5)"); // fibonacci number
    assert(42, ({ int a = 42; int* b = &a; *b; }), "({ int a = 42; int* b = &a; *b; })");
    assert(42, ({ int a = 42; *&a; }), "({ int a = 42; *&a; })");
    assert(42, ({ int a = 42; int* b = &a; int** c = &b; **c; }), "({ int a = 42; int* b = &a; int** c = &b; **c; })");
    assert(84, ({ int a = 42; int* b = &a; *b = a * 2; a; }), "({ int a = 42; int* b = &a; *b = a * 2; a; })");
    assert(42, ({ int a = 42; int b = 5; *(&b+1); }), "({ int a = 42; int b = 5; *(&b+1); })");
    assert(53, ({ int a = 42; int b = 5; *(&a-1) = 53; b; }), "({ int a = 42; int b = 5; *(&a-1) = 53; b; })");
    assert(53, ({ int a = 42; int b = 5; *(&b+1) = 53; a; }), "({ int a = 42; int b = 5; *(&b+1) = 53; a; })");
    assert(6, ({ int sum = 0; int i = 1; for (; i < 4; i = i + 1) sum = sum + i; sum; }), "({ int sum = 0; int i = 1; for (; i < 4; i = i + 1) sum = sum + i; return sum; })");
    assert(3, ({ int ar[2]; int* p = ar; *p = 3; *ar; }), "({ int ar[2]; int* p = ar; *p = 3; *ar; })");
    assert(3, ({ int ar[2]; int* p = ar; *(p + 1) = 3; *(ar + 1); }), "({ int ar[2]; int* p = ar; *(p + 1) = 3; *(ar + 1); })");
    assert(5, ({ int ar[2]; int* p = ar; *p = 2; *(p + 1) = 3; *ar + *(ar + 1); }), "({ int ar[2]; int* p = ar; *p = 2; *(p + 1) = 3; *ar + *(ar + 1); })");
    assert(1, ({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *ar; }), "({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *ar; })");
    assert(2, ({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *(ar + 1); }), "({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *(ar + 1); })");
    assert(3, ({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *(ar + 2); }), "({ int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; *(ar + 2); })");
    assert(42, ({ int a = 0; gg(&a); a; }), "({ int a = 0; gg(&a); a; })");
    assert(45, ({ int ar[10]; int i = 0; for (; i < 10; i = i + 1) { *(ar + i) = i; } int sum = 0; for (i = 0; i < 10; i = i + 1) { sum = sum + *(ar + i); } sum; }), "({ int ar[10]; int i = 0; for (; i < 10; i = i + 1) { *(ar + i) = i; } int sum = 0; for (i = 0; i < 10; i = i + 1) { sum = sum + *(ar + i); } sum; })");
    assert(45, ({ int ar[10]; int i = 0; for (; i < 10; i = i + 1) *(ar + i) = i; sum(ar, 10); }), "({ int ar[10]; int i = 0; for (; i < 10; i = i + 1) *(ar + i) = i; sum(ar, 10); })");
    assert(9, ({ int ar[2][3]; int sum = 0; int i = 0; for (; i < 2; i = i + 1) { int j = 0; for (; j < 3; j = j + 1) { *(*(ar + i) + j) = i + j; sum = sum + *(*(ar + i) + j); } } sum; }), "({ int ar[2][3]; int sum = 0; int i = 0; for (; i < 2; i = i + 1) { int j = 0; for (; j < 3; j = j + 1) { *(*(ar + i) + j) = i + j; sum = sum + *(*(ar + i) + j); } } sum; })");
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
    assert(8, ({ int a; sizeof(a); }), "({ int a; sizeof(a); })");
    assert(8, ({ int a; sizeof a; }), "({ int a; sizeof a; })");
    assert(8, ({ int* p; sizeof p; }), "({ int* p; sizeof p; })");
    assert(3 * 8, ({ int ar[3]; sizeof ar; }), "({ int ar[3]; sizeof ar; })");
    assert(3 * 5 * 8, ({int ar[3][5]; sizeof ar; }), "({int ar[3][5]; return sizeof ar; })");
    assert(5 * 8, ({ int ar[3][5]; sizeof *ar; }), "({ int ar[3][5]; sizeof *ar; })");
    assert(8, ({ int ar[3][5]; sizeof **ar; }), "({ int ar[3][5]; sizeof **ar; })");
    assert(8 + 1, ({ int ar[3][5]; sizeof(**ar) + 1; }), "({ int ar[3][5]; sizeof(**ar) + 1; })");
    assert(8 + 1, ({ int ar[3][5]; sizeof **ar + 1; }), "({ int ar[3][5]; return sizeof **ar + 1; })");
    assert(8, ({ int ar[3][5]; sizeof(**ar + 1); }), "({ int ar[3][5]; sizeof(**ar + 1); })");
    assert(42, ({ int ar[2]; 2[ar] = 42; ar[2]; }), "({ int ar[2]; 2[ar] = 42; ar[2]; })");
    assert(0, g, "g");
    assert(42, ({ g = 42; g; }), "({ g = 42; g; })");
    assert(1, ({ int i = 0; for (; i < sizeof gr; i = i + 1) gr[i] = i + 1; gr[0]; }), "({ int i = 0; for (; i < sizeof gr; i = i + 1) gr[i] = i + 1; gr[0]; })");
    assert(2, gr[1], "gr[1]");
    assert(3, gr[2], "gr[2]");
    assert(1, ({ char c = 1; c; }), "({ char c = 1; c; })");
    assert(1, ({ char c1 = 1; char c2 = 2; c1; }), "({ char c1 = 1; char c2 = 2; c1; })");
    assert(2, ({ char c1 = 1; char c2 = 2; c2; }), "({ char c1 = 1; char c2 = 2; c2; })");
    assert(1, ({ char x; sizeof x; }), "({ char x; sizeof x; })");
    assert(10, ({ char ar[10]; sizeof ar; }), "({ char ar[10]; return sizeof ar; })");
    assert(1, sub3(7, 3, 3), "sub3(7, 3, 3)");
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
    assert(27, "\e"[0], "\"\\e\"[0]");
    assert(0, "\0"[0], "\\0[0]");
    assert(106, "\j"[0], "\\j[0]");
    assert(107, "\k"[0], "\\k[0]");
    assert(108, "\l"[0], "\\l[0]");
    assert(92, "\\"[0], "a");
    assert(42, ({ int a = 42; { int a = 32; } a; }), "({ int a = 42; { int a = 32; } a; })");
    assert(32, ({ int a = 42; { a = 32; } a; }), "({ int a = 42; { a = 32; } a; })");
    assert(2, ({ int ar[5]; int* p = ar + 2; p - ar; }), "({ int ar[5]; int* p = ar + 2; p - ar; })");
    assert(1, ({ struct { int a; int b; } x; x.a = 1; x.b = 2; x.a; }), "({ struct { int a; int b; } x; x.a = 1; x.b = 2; x.a; })");
    assert(2, ({ struct { int a; int b; } x; x.a = 1; x.b = 2; x.b; }), "({ struct { int a; int b; } x; x.a = 1; x.b = 2; x.b; })");
    assert(1, ({ struct { char a; int b; char c; } x; x.a = 1; x.b = 2; x.c = 3; x.a; }), "({ struct { char a; int b; char c; } x; x.a = 1; x.b = 2; x.c = 3; x.a; })");
    assert(2, ({ struct { char a; int b; char c; } x; x.a = 1; x.b = 2; x.c = 3; x.b; }), "({ struct { char a; int b; char c; } x; x.a = 1; x.b = 2; x.c = 3; x.b; })");
    assert(3, ({ struct { char a; int b; char c; } x; x.a = 1; x.b = 2; x.c = 3; x.c; }), "({ struct { char a; int b; char c; } x; x.a = 1; x.b = 2; x.c = 3; x.c; })");
    assert(0, ({ struct { int a; int b; } ar[3]; int* p = ar; p[0] = 0; ar[0].a; }), "({ struct { int a; int b; } ar[3]; int* p = ar; p[0] = 0; ar[0].a; })");
    assert(1, ({ struct { int a; int b; } ar[3]; int* p = ar; p[1] = 1; ar[0].b; }), "({ struct { int a; int b; } ar[3]; int* p = ar; p[1] = 1; ar[0].b; })");
    assert(2, ({ struct { int a; int b; } ar[3]; int* p = ar; p[2] = 2; ar[1].a; }), "({ struct { int a; int b; } ar[3]; int* p = ar; p[2] = 2; ar[1].a; }),");
    assert(3, ({ struct { int a; int b; } ar[3]; int* p = ar; p[3] = 3; ar[1].b; }), "({ struct { int a; int b; } ar[3]; int* p = ar; p[3] = 3; ar[1].b; })");
    assert(6, ({ struct { int a[3]; int b[5]; } x; int*p = &x; x.a[0] = 6; p[0]; }), "({ struct { int a[3]; int b[5]; } x; int* p = &x; x.a[0] = 6; p[0]; )}");
    assert(7, ({ struct {int a[3]; int b[5]; } x; int* p = &x; x.b[0] = 7; p[3]; }), "({ struct { int a[3]; int b[5]; } x; int*p = &x; x.b[0] = 7; p[3]; })");
    assert(6, ({ struct { struct { int b; } a; } x; x.a.b=6; x.a.b; }), "({ struct { struct { int b; } a; } x; x.a.b=6; x.a.b; })");
    assert(8, ({ struct { int a; } x; sizeof(x); }), "({ struct { int a; } x; sizeof(x); )}");
    assert(16, ({ struct { int a; int b; } x; sizeof(x); }), "({ struct { int a; int b; } x; sizeof(x); )}");
    assert(24, ({ struct {int ar[3];} x; sizeof(x); }), "({ struct { int ar[3]; } ar; sizeof(ar); })");
    assert(32, ({ struct { int a; } x[4]; sizeof(x); }), "({ struct { int a; } x[4]; sizeof(x); })");
    assert(48, ({ struct { int ar[3]; } x[2]; sizeof(x); }), "({ struct { int ar[3]; } x[2]; sizeof(x) }; })");
    assert(2, ({ struct { char a; char b; } x; sizeof(x); }), "({ struct { char a; char b; } x; sizeof(x); })");
    assert(16, ({ struct { char a; int b; } x; sizeof(x); }), "({ struct { char a; int b; } x; sizeof(x); })");
    assert(16, ({ struct { int a; char b; } x; sizeof(x); }), "({ struct { int a; char b; } x; sizeof(x); })");
    assert(1, ({ char c; _Alignof c; }), "({ char c; _Alignof c; })");
    assert(8, ({ int x; _Alignof x; }), "({ int x; _Alignof x; })");
    assert(8, ({ int* x; _Alignof x; }), "({ int* x; _Alignof x; })");
    assert(1, ({ char ar[10]; _Alignof ar; }), "({ char ar[10]; _Alignof ar; })");
    assert(8, ({ int ar[10]; _Alignof ar; }), "({ int ar[10]; _Alignof ar; })");
    assert(8, ({ struct { char a; int b; } x; _Alignof x; }), "({ struct { char a; int b; } x; _Alignof x; })");
    assert(8, ({ _Alignof add(1, 2); }), "({ _Alignof add(1, 2); })");
    assert(15, ({ int a; char b; int c; int p1 = &c; int p2 = &b; p2 - p1; }), "({ int a; char b; int c; &c - &b; })");
    assert(1, ({ int a; char b; int p1 = &a; int p2 = &b; p1 - p2; }), "({ int a; int b; int p1 = &a; int p2 = &b; p2 - p1; })");

    return 0;
}
