// This is a c compiler test file. This comment itself is a line comment test.
/*
 * This comment is also a block comment test.
 */

int printf();
int exit();
int strcmp(char* p, char* q);
int test_num;

int g;
int gr[3];
//int (*gpa)[3];
char gc = 1;
short gsh = 2;
int gi = 3;
long gl = 4;
int* gp = &gi;
char* gstr = "abc";

int assert(long expected, long actual, char* code)
{
    if (expected == actual) {
        printf("[OK]: test #%ld: \'%s\' => %d\n", test_num, code, actual);
        test_num = test_num + 1;
        return 0;
    } else {
        printf("[Failed]: test #%ld: \'%s\' => %d, but expected %d\n", test_num, code, actual, expected);
        exit(1);
    }
}

int f() { return 42; }
int add(int x, int y) { return x + y; }
int rec(int a) { if (a == 0) return 42; return rec(a - 1); } 
int fib(int n) { if (n == 0) return 1; else if (n == 1) return 1; else if (n >= 2) return fib(n - 1) + fib(n - 2); else return 0; }
int gg(int* p) { *p = 42; return 0; } 
int sum(int* p, int n) { int s = 0; for (int i = 0; i < n; i = i + 1) s = s + *(p + i); return s; }
int sub3(int a, int b, int c) { return a - b - c; }
int sub3_short(short a, short b, short c) { return a - b - c; }
int sub3_long(long a, long b, long c) { return a - b - c; }
/*int ptr2ar(int (*p)[3]) { int i = 0; for (; i < sizeof *p / sizeof **p; i = i + 1) p[0][i] = i + 1; return 0; }
static int static_fun() { return 42; }*/

int main()
{
    test_num = 1;
    assert(42, ({ int a = 42; a; }), "({ int a = 42; a; })");
    assert(42, ( { int a = 42; a; } ), "( { int a = 42; a; } )");
    assert(44, ({ int a = 42; int b = 2; a + b; }), "({ int a = 42; int b = 2; a + b; })");
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
    assert(6, ({ int s = 0; int i = 1; for (; i < 4; i = i + 1) s = s + i; s; }), "({ int s = 0; int i = 1; for (; i < 4; i = i + 1) s = s + i; return s; })");
    assert(3, ({ int a = 0; for(; a < 3; a = a + 1); a; }), "({ int a = 0; for(; a < 3; a = a + 1); a; })");
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
    assert(4, ({ int a; sizeof(a); }), "({ int a; sizeof(a); })");
    assert(4, ({ int a; sizeof a; }), "({ int a; sizeof a; })");
    assert(8, ({ int* p; sizeof p; }), "({ int* p; sizeof p; })");
    assert(3 * 4, ({ int ar[3]; sizeof ar; }), "({ int ar[3]; sizeof ar; })");
    assert(3 * 5 * 4, ({int ar[3][5]; sizeof ar; }), "({int ar[3][5]; return sizeof ar; })");
    assert(5 * 4, ({ int ar[3][5]; sizeof *ar; }), "({ int ar[3][5]; sizeof *ar; })");
    assert(4, ({ int ar[3][5]; sizeof **ar; }), "({ int ar[3][5]; sizeof **ar; })");
    assert(4 + 1, ({ int ar[3][5]; sizeof(**ar) + 1; }), "({ int ar[3][5]; sizeof(**ar) + 1; })");
    assert(4 + 1, ({ int ar[3][5]; sizeof **ar + 1; }), "({ int ar[3][5]; return sizeof **ar + 1; })");
    assert(8, ({ int ar[3][5]; sizeof(**ar + 1); }), "({ int ar[3][5]; sizeof(**ar + 1); })");
    assert(42, ({ int ar[2]; 2[ar] = 42; ar[2]; }), "({ int ar[2]; 2[ar] = 42; ar[2]; })");
    assert(0, g, "g"); 
    assert(42, ({ g = 42; g; }), "({ g = 42; g; })");
    assert(1, ({ int i = 0; for (; i < sizeof gr / sizeof gr[0]; i = i + 1) gr[i] = i + 1; gr[0]; }), "({ int i = 0; for (; i < sizeof gr / sizeof gr[0]; i = i + 1) gr[i] = i + 1; gr[0]; })");
    assert(2, gr[1], "gr[1]");
    assert(3, gr[2], "gr[2]");
    assert(1, ({ char c = 1; c; }), "({ char c = 1; c; })");
    assert(1, ({ char c1 = 1; char c2 = 2; c1; }), "({ char c1 = 1; char c2 = 2; c1; })");
    assert(2, ({ char c1 = 1; char c2 = 2; c2; }), "({ char c1 = 1; char c2 = 2; c2; })");
    assert(1, ({ char x; sizeof x; }), "({ char x; sizeof x; })");
    assert(10, ({ char ar[10]; sizeof ar; }), "({ char ar[10]; return sizeof ar; })");
    assert(1, sub3(7, 3, 3), "sub3(7, 3, 3)");
    assert(92, "\\"[0], "a");
    assert(42, ({ int a = 42; { int a = 32; } a; }), "({ int a = 42; { int a = 32; } a; })");
    assert(32, ({ int a = 42; { a = 32; } a; }), "({ int a = 42; { a = 32; } a; })");
    assert(2, ({ int ar[5]; int* p = ar + 2; p - ar; }), "({ int ar[5]; int* p = ar + 2; p - ar; })");
    /*assert(1, ({ struct { int a; int b; } x; x.a = 1; x.b = 2; x.a; }), "({ struct { int a; int b; } x; x.a = 1; x.b = 2; x.a; })");
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
    assert(4, ({ struct { int a; } x; sizeof(x); }), "({ struct { int a; } x; sizeof(x); )}");
    assert(8, ({ struct { int a; int b; } x; sizeof(x); }), "({ struct { int a; int b; } x; sizeof(x); )}");
    assert(12, ({ struct {int ar[3];} x; sizeof(x); }), "({ struct { int ar[3]; } ar; sizeof(ar); })");
    assert(16, ({ struct { int a; } x[4]; sizeof(x); }), "({ struct { int a; } x[4]; sizeof(x); })");
    assert(24, ({ struct { int ar[3]; } x[2]; sizeof(x); }), "({ struct { int ar[3]; } x[2]; sizeof(x) }; })");
    assert(2, ({ struct { char a; char b; } x; sizeof(x); }), "({ struct { char a; char b; } x; sizeof(x); })");
    assert(8, ({ struct { char a; int b; } x; sizeof(x); }), "({ struct { char a; int b; } x; sizeof(x); })");
    assert(8, ({ struct { int a; char b; } x; sizeof(x); }), "({ struct { int a; char b; } x; sizeof(x); })");
    assert(1, ({ char c; _Alignof c; }), "({ char c; _Alignof c; })");
    assert(4, ({ int x; _Alignof x; }), "({ int x; _Alignof x; })");
    assert(8, ({ int* x; _Alignof x; }), "({ int* x; _Alignof x; })");
    assert(1, ({ char ar[10]; _Alignof ar; }), "({ char ar[10]; _Alignof ar; })");
    assert(4, ({ int ar[10]; _Alignof ar; }), "({ int ar[10]; _Alignof ar; })");
    assert(4, ({ struct { char a; int b; } x; _Alignof x; }), "({ struct { char a; int b; } x; _Alignof x; })");
    assert(4, ({ _Alignof add(1, 2); }), "({ _Alignof add(1, 2); })");
    assert(7, ({ char a; int b; int p1 = &b; int p2 = &a; p2 - p1; }), "({ int a; char b; int c; &c - &b; })");
    assert(1, ({ int a; char b; int p1 = &a; int p2 = &b; p1 - p2; }), "({ int a; int b; int p1 = &a; int p2 = &b; p2 - p1; })");
    assert(8, ({ struct X { int a; int b; } x; struct X y; sizeof y; }), "({ struct X { int a; int b; } x; struct X y; sizeof y; })");
    assert(8, ({ struct X { int a; int b; }; struct X y; sizeof y; }), "({ struct X { int a; int b; } x; struct X y; sizeof y; })");
    assert(2, ({ struct X { char ar[2]; }; { struct X { char ar[4]; }; } struct X x; sizeof x; }), "({ struct X { char ar[2]; }; { struct X { char ar[4]; }; } struct X x; sizeof x; })");
    assert(3, ({ struct x { int a; }; int x = 1; struct x y; y.a = 2; x + y.a; }), "({ struct x { int a; } int x = 1; struct x y; y.a = 2; x + y.a; })");
    assert(42, ({ struct X { struct { int a; } x; }; struct X x; x.x.a = 42; x.x.a; }), "({ struct X { struct { int a; } x; }; struct X x; x.x.a = 42; x.x.a; })");
    assert(42, ({ struct X { char a; } x; struct X* p = &x; x.a = 42; p->a; }), "({ struct X { char a; } x; struct X* p = &x; x.a = 42; p->a; })");
    assert(42, ({ struct X { char a; } x; struct X* p = &x; x.a = 42; p->a; }), "({ struct X { char a; } x; struct X* p = &x; p->a = 42; x.a; })");
    assert(42, ({ struct X { int a; }* p; struct X x; p = &x; x.a = 42; p->a; }), "({ struct X { int a; }* p; struct X x; p = &x; x.a = 42; p->a; })");
    assert(42, ({ struct X { int a; }* p; struct X x; p = &x; p->a = 42; x.a; }), "({ struct X { int a; }* p; struct X x; p = &x; x.a = 42; p->a; })");
    assert(42, ({ struct X { struct Y { int a; }* py; }; struct X x; struct Y y; y.a = 42; x.py = &y; x.py-> a; }), "({ struct X { struct Y { int a; }* py; }; struct X x; struct Y y; y.a = 42; x.py = &y; x.py-> a; })");
    assert(42, ({ typedef int t; t x = 42; x; }), "typedef int t; t x = 42; x;");
    assert(42, ({ typedef struct { int a; } t; t x; x.a = 42; x.a; }), "typedef struct { int a; } t; t x; x.a = 42; x.a;");
    assert(42, ({ typedef int t; t t = 42; t; }), "typedef int t; t t = 42; t;");
    assert(42, ({ typedef struct { int a; } t; { typedef int t; } t x; x.a = 42; x.a; }), "typedef struct { int a; } t; { typedef int t; } t x; x.a = 42; x.a;");
    assert(42, ({ typedef int art[2]; typedef art g; g ar; ar[0] = 0; ar[1] = 42; ar[1]; }), "({ typedef int art[2]; typedef art g; g ar; ar[0] = 0; ar[1] = 42; ar[1]; })");
    assert(42, ({ typedef int t; typedef int t; t a = 42; a; }), "({ typedef int t; typedef int t; t a = 42; a; })");
    assert(2, ({ short a; sizeof a; }), "({ short a; sizeof a; })");
    assert(4, ({ struct { char a; short b; } x; sizeof x; }), "({ struct { char a; short b; } x; sizeof x; })");
    assert(8, ({ long a; sizeof a; }), "({ long a; sizeof a; })");
    assert(16, ({ struct { char a; long b; } x; sizeof x; }), "({ struct { char a; long b; } x; sizeof x; })");
    assert(1, sub3_short(7, 3, 3), "sub3_short(7, 3, 3)");
    assert(1, sub3_long(7, 3, 3), "sub3_long(7, 3, 3)");
    assert(4, ({ short int a; int short b; sizeof a + sizeof b; }), "({ short int a; int short b; sizeof a + sizeof b; })");
    assert(16, ({ long int a; int long b; sizeof a + sizeof b; }), "({ long int a; int long b; sizeof a + sizeof b; })");
    assert(32, ({ typedef int* p[4]; p a; sizeof a; }), "({ typedef int* p[4]; sizeof p; })");
    assert(8, ({ typedef int (*pp)[4]; pp a; sizeof a; }), "({ typedef int (*p)[4]; sizeof p; })");
    assert(1, ({ gpa = gr; (*gpa)[0]; }), "({ gpa = gr; (*gpa)[0]; })");
    assert(2, gpa[0][1], "(*gpa)[1]");
    assert(3, gpa[0][2], "(*gpa)[2]");
    assert(42, ({ int* ar[3]; int x; ar[0] = &x; x = 42; ar[0][0]; }), "({ int* ar[3]; int x; ar[0] = &x; x = 42; ar[0][0]; })");
    assert(42, ({ int ar[3]; int (*p)[3] = ar; p[0][0] = 42; ar[0]; }), "({ int ar[3]; int (*p)[3] = ar; p[0][0] = 42; ar[0]; })");
    assert(6, ({ int ar[3]; ptr2ar(&ar); sum(ar, sizeof ar / sizeof *ar); }), "({ int ar[3]; ptr2ar(&ar); sum(ar, sizeof ar / sizeof **ar); }");
    assert(42, ({ struct { int (*p)[3]; } x; int ar[3]; x.p = &ar; (*x.p)[0] = 42; ar[0]; }), "({ struct { int (*p)[3]; } x; int ar[3]; x.p = &ar; (*x.p)[0]     = 42; ar[0]; })");
    { void* x; }
    assert(0, ({ _Bool x = 0; x; }), "({ _Bool x = 0; x; })");
    assert(1, ({ _Bool x = 1; x; }), "({ _Bool x = 1; x; })");
    assert(1, ({ _Bool x = 2; x; }), "({ _Bool x = 2; x; })");
    assert(8, ({ long long x; sizeof x; }), "({ long long x; sizeof x; })");
    assert(8, ({ long long int x; sizeof x; }), "({ long long int x; sizeof x; })");
    assert(8, ({ long int long x; sizeof x; }), "({ long int long x; sizeof x; })");
    assert(8, ({ int long long x; sizeof x; }), "({ int long long x; sizeof x; })");
    assert(1, sizeof(char), "sizeof(char)");
    assert(1, sizeof(_Bool), "sizeof(_Bool)");
    assert(2, sizeof(short), "sizeof(short)");
    assert(2, sizeof(short int), "sizeof(short int)");
    assert(2, sizeof(int short), "sizeof(int short)");
    assert(4, sizeof(int), "sizeof(int)");
    assert(8, sizeof(long), "sizeof(long)");
    assert(8, sizeof(long int), "sizeof(long int)");
    assert(8, sizeof(int long), "sizeof(int long)");
    assert(8, sizeof(long long), "sizeof(long long)");
    assert(8, sizeof(long long int), "sizeof(long long int)");
    assert(8, sizeof(long int long), "sizeof(long int long)");
    assert(8, sizeof(int long long), "sizeof(int long long)");
    assert(8, sizeof(char*), "sizeof(char*)");
    assert(8, sizeof(int*), "sizeof(int*)");
    assert(8, sizeof(long*), "sizeof(long*)");
    assert(8, sizeof(int**), "sizeof(int**)");
    assert(8, sizeof(int (*)[4]), "sizeof(int (*)[4])");
    assert(32, sizeof(int* [4]), "sizeof(int* [4])");
    assert(16, sizeof(int[4]), "sizeof(int[4])");
    assert(48, sizeof(int[3][4]), "sizeof(int[3][4])");
    assert(8, sizeof(struct { int a; int b; }), "sizeof(struct { int a; int b; })");
    assert(131585, (int)8590066177, "(int)8590066177");
    assert(513, (short)8590066177, "(short)8590066177");
    assert(1, (char)8590066177, "(char)8590066177");
    assert(1, (_Bool)1, "(_Bool)1");
    assert(1, (_Bool)2, "(_Bool)2");
    assert(0, (_Bool)(char)256, "(_Bool)(char)256");
    assert(1, (long)1, "(long)1");
    assert(0, (long)&*(int *)0, "(long)&*(int *)0");
    assert(42, ({ int a = 42 ; long b = (long)&a; *(int*)b; }), "int a = 42; long b = (long)&a; *(int*)b");
    assert(2147483648, ({ int a = 2147483647; long b = a + 1; b; }), " ({ int a = 2147483647; long b = a + 1; b; })");*/
    assert(97, 'a', "'a'");
    assert(10, '\n', "\'\\n\'");/*
    assert(0, ({ enum { zero, one, two }; zero; }), "enum { zero, one, two }; zero;");
    assert(1, ({ enum { zero, one, two }; one; }), "enum { zero, one, two }; one;");
    assert(2, ({ enum { zero, one, two }; two; }), "enum { zero, one, two }; two;");
    assert(5, ({ enum { five = 5, six, seven }; five; }), "enum { five = 5, six, seven }; five;");
    assert(6, ({ enum { five = 5, six, seven }; six; }), "enum { five = 5, six, seven }; six;");
    assert(0, ({ enum { zero, five = 5, three = 3, four }; zero; }), "enum { zero, five = 5, three = 3, four }; zero;");
    assert(5, ({ enum { zero, five = 5, three = 3, four }; five; }), "enum { zero, five = 5, three = 3, four }; five;");
    assert(3, ({ enum { zero, five = 5, three = 3, four }; three; }), "enum { zero, five = 5, three = 3, four }; three;");
    assert(4, ({ enum { zero, five = 5, three = 3, four }; four; }), "enum { zero, five = 5, three = 3, four }; four;");
    assert(4, ({ enum { zero, one, two } x; sizeof x; }), "enum { zero, one, two } x; sizeof x;");
    assert(4, ({ enum t { zero, one, two }; enum t y; sizeof y; }), "enum t { zero, one, two }; enum t y; sizeof y;");
    assert(0, ({ typedef enum { zero } e; e y = zero; y; }), " ({ typedef enum { zero } e; e y = zero; y; })");
    assert(42, (1, 2, 42), "(1, 2, 42)");*/
    //assert(43, ({ enum { a = 14 + 14 + 14, b }; b; }), "({ enum { a = 14 + 14 + 14, b }; b; })");
/*    assert(1, sizeof(signed char), "sizeof(signed char)");
    assert(1, sizeof(char signed), "sizeof(char signed)");
    assert(4, sizeof(signed int), "sizeof(signed int)");
    assert(4, sizeof(int signed), "sizeof(int signed)");
    assert(8, sizeof(signed long), "sizeof(signed long)");
    assert(8, sizeof(long signed), "sizeof(long signed)");
    assert(8, sizeof(signed long int), "sizeof(signed long int)");
    assert(8, sizeof(signed int long), "sizeof(signed int long)");
    assert(8, sizeof(long signed int), "sizeof(long signed int)");
    assert(8, sizeof(long int signed), "sizeof(long int signed)");
    assert(8, sizeof(int signed long), "sizeof(int signed long)");
    assert(8, sizeof(int long signed), "sizeof(int long signed)");
    assert(8, sizeof(long long signed int), "sizeof(long long signed int)");
    assert(8, sizeof(long long int signed), "sizeof(long long signed int)");
    assert(8, sizeof(long signed long int), "sizeof(long signed long int)");
    assert(8, sizeof(long signed int long), "sizeof(long signed int long)");
    assert(8, sizeof(long int long signed), "sizeof(long int long signed)");
    assert(8, sizeof(long int signed long), "sizeof(long int signed long)");
    assert(8, sizeof(int long long signed), "sizeof(int long long signed)");
    assert(8, sizeof(int signed long long), "sizeof(int signed long long)");
    assert(8, sizeof(int long signed long), "sizeof(int long signed long)");
    assert(8, sizeof(signed long long int), "sizeof(signed long long int)");
    assert(8, sizeof(signed long int long), "sizeof(signed long int long)");
    assert(8, sizeof(signed int long long), "sizeof(signed int long long)");
    assert(42, static_fun(), "static_fun()");
    assert(42, ({ register int x = 42; x; }), "({ register int x = 42; x; })");
    assert(42, ({ auto int x = 42; x; }), "({ auto int x = 42; x; })");
    assert(42, ({ register struct { int x; } x; x.x = 42; x.x; }), "({ register struct { int x; } x; x.x = 42; x.x; })");
    assert(42, ({ register struct X { int x; }* p; struct X x; p = &x; p->x = 42; x.x; }), "({ register struct X { int x; }* p; struct X x; p = &x; p->x = 42; x.x; })");
    assert(42, ({ auto struct { int x; } x; x.x = 42; x.x; }), "({ auto struct { int x; } x; x.x = 42; x.x; })");
    assert(42, ({ auto struct X { int x; }* p; struct X x; p = &x; p->x = 42; x.x; }), "({ register struct X { int x; }* p; struct X x; p = &x; p->x = 42; x.x; })");*/
    assert(42, ({ int i = 42; for (int i = 0; i < 10; ++i); i; }), "({ int i = 42; for (int i = 0; i < 10; ++i); i; })");
    /*assert(42, ({ int i = 42; for (auto int i = ({ int i = 0; for (; i < 10; ++i); i; }); i > 0; --i); i; }), "for (int i = ({ int i = 0; for (; i < 10; ++i); i; }); i > 0; --i); i; })");
    assert(42, ({ for (struct { int x; } x; 0;); 42; }), "({ for (struct { int x; } x; 0;); 42; })");*/
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
    /*assert(4, ({ struct X *a; struct X { int x; }; sizeof(struct X); }), " ({ struct X *a; struct X { int x; }; sizeof(struct X); })");
    assert(42, ({ struct X { struct X* next; int x; } a; struct X b; b.x = 42; a.next = &b; a.next->x; }), "({ struct X { struct X* next; int x; } a; struct X b; b.x = 42; a.next = &b; a.next->x; })");*/
    assert(3, ({ int i = 0; for (; i < 10; ++i) { if (i == 3) break; } i; }), "({ int i = 0; for (; i < 10; ++i) { if (i == 3) break; } i; })");
    assert(3, ({ int i = 0; for (; i < 10; ++i) { for (;;) break; if (i == 3) break; } i; }), "({ int i = 0; for (; i < 10; ++i) { for (;;) break; if (i == 3) break; } i; })");
    assert(4, ({ int i = 0; while (1) { if (i++ == 3) break; } i; }), "({ int i = 0; while (1) { if (i++ == 3) break; } i; })");
    assert(4, ({ int i = 0; while (1) { for (;;) break; if (i++ == 3) break; } i; }), "({ int i = 0; while (1) { for (;;) break; if (i++ == 3) break; } i; })");
    assert(3, ({ int i = 0; goto a; a: ++i; b: ++i; c: ++i; i; }), "({ int i = 0; goto a; a: ++i; b: ++i; c: ++i; i; })");
    assert(2, ({ int i = 0; goto e; d: ++i; e: ++i; f: ++i; i; }), "({ int i = 0; goto e; d: ++i; e: ++i; f: ++i; i; })");
    assert(1, ({ int i = 0; goto i; g: ++i; h: ++i; i: ++i; i; }), "({ int i = 0; goto i; g: ++i; h: ++i; i: ++i; i; })");
    assert(42, ({ int i = 0; switch (0) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; }), "({ int i = 0; switch (0) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; })");
    assert(43, ({ int i = 0; switch (1) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; }), "({ int i = 0; switch (1) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; })");
    assert(44, ({ int i = 0; switch (2) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; }), "({ int i = 0; switch (2) { case 0: i = 42; break; case 1: i = 43; break; case 2: i = 44; break; } i; })");
    assert(42, ({ int i = 42; switch (3) { case 0: i = 5; break; case 1: i = 6; break; case 2: i = 7; break; } i; }), "({ int i = 42; switch (3) { case 0: i = 5; break; case 1: i = 6; break; case 2: i = 7; break; } i; })");
    assert(42, ({ int i = 0; switch (0) { case 0: i = 42; break; default: i = 43; } i; }), "({ int i = 0; switch (0) { case 0: i = 42; break; default: i = 43; } i; })");
    assert(43, ({ int i = 0; switch (1) { case 0: i = 42; break; default: i = 43; } i; }), "({ int i = 0; switch (0) { case 0: i = 42; break; default: i = 43; } i; })");
    assert(42, ({ int i = 0; switch (1) { case 0: 0; case 1: 0; case 2: 0; i = 42; } i; }), "({ int i = 0; switch (1) { case 0: 0; case 1: 0; case 2: 0; i = 42; } i; })");
    assert(0, ({ int i = 0; switch (3) { case 0: 0; case 1: 0; case 2: 0; i = 42; } i; }), "({ int i = 0; switch (3) { case 0: 0; case 1: 0; case 2: 0; i = 42; } i; })");
    assert(42, ({ int i = 40; switch (0) { case 0: ++i; case 1: ++i; } i; }), "({ int i = 40; switch (0) { case 0: ++i; case 1: ++i; } i; })");
    assert(41, ({ int i = 40; switch (i) { case 20 * 2: ++i; } i; }), "({ int i = 40; switch (i) { case 20 * 2: ++i; } i; })");
    //assert(0, ({ int ar[]; 0; }), "({ int ar[]; 0; })");
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
    assert(0, ({ int ar[1][1][1] = {{{}}}; ar[0][0][0]; }), "({ int ar[1][1][1] = {{{}}}; ar[0][0][0]; })");/*
    assert(3, ({ int ar[] = { 0, 1, 2, 3 }; ar[3]; }), "({ int ar[] = { 0, 1, 2, 3 }; ar[3]; })");
    assert(16, ({ int ar[] = { 0, 1, 2, 3 }; sizeof ar; }), "({ int ar[] = { 0, 1, 2, 3 }; sizeof ar; })");
    assert(1, ({ struct { int a; int b; int c; } x = { 1, 2, 3 }; x.a; }), "({ struct { int a; int b; int c; } x = { 1, 2, 3 }; x.a; })");
    assert(2, ({ struct { int a; int b; int c; } x = { 1, 2, 3 }; x.b; }), "({ struct { int a; int b; int c; } x = { 1, 2, 3 }; x.b; })");
    assert(3, ({ struct { int a; int b; int c; } x = { 1, 2, 3 }; x.c; }), "({ struct { int a; int b; int c; } x = { 1, 2, 3 }; x.c; })");
    assert(1, ({ struct { int a; int b; int c; } x = { 1 }; x.a; }), "({ struct { int a; int b; int c; } x = { 1 }; x.a; })");
    assert(0, ({ struct { int a; int b; int c; } x = { 1 }; x.b; }), "({ struct { int a; int b; int c; } x = { 1 }; x.b; })");
    assert(0, ({ struct { int a; int b; int c; } x = { 1 }; x.c; }), "({ struct { int a; int b; int c; } x = { 1 }; x.c; })");
    assert(1, ({ struct { int a; int b; } x[2] = { { 1, 2 }, { 3, 4 } }; x[0].a; }), "({ struct { int a; int b; } x[2] = { { 1, 2 }, { 3, 4 } }; x[0].a; })");
    assert(2, ({ struct { int a; int b; } x[2] = { { 1, 2 }, { 3, 4 } }; x[0].b; }), "({ struct { int a; int b; } x[2] = { { 1, 2 }, { 3, 4 } }; x[0].b; })");
    assert(3, ({ struct { int a; int b; } x[2] = { { 1, 2 }, { 3, 4 } }; x[1].a; }), "({ struct { int a; int b; } x[2] = { { 1, 2 }, { 3, 4 } }; x[1].a; })");
    assert(4, ({ struct { int a; int b; } x[2] = { { 1, 2 }, { 3, 4 } }; x[1].b; }), "({ struct { int a; int b; } x[2] = { { 1, 2 }, { 3, 4 } }; x[1].b; })");
    assert(0, ({ struct { int a; int b; } x[2] = { { 1, 2 } }; x[1].a; }), "({ struct { int a; int b; } x[2] = { { 1, 2 } }; x[1].a; })");
    assert(0, ({ struct { int a; int b; } x[2] = { { 1, 2 } }; x[1].b; }), "({ struct { int a; int b; } x[2] = { { 1, 2 } }; x[1].b; })");
    assert(0, ({ struct { int a; int b; } x = {}; x.a; }), "({ struct { int a; int b; } x = {}; x.a; })");
    assert(0, ({ struct { int a; int b; } x = {}; x.b; }), "({ struct { int a; int b; } x = {}; x.b; })");*/
    assert(1, gc, "gc");
    assert(2, gsh, "gsh");
    assert(3, gi, "gi");
    assert(4, gl, "gl");
    assert(3, *gp, "*gp");
    assert(0, strcmp(gstr, "abc"), "strcmp(gstr, \"abc\")");
    
    printf("All tests are passed!\n");

    return 0;
}
