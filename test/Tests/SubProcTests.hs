{-# LANGUAGE OverloadedStrings #-}
module Tests.SubProcTests (
    exec
) where

import           Data.Char         (ord)
import           Numeric.Natural

import qualified Tests.Test1       as StatementEqual
import qualified Tests.Test2       as LinkFuncRet
import qualified Tests.Test3       as LinkFuncStdOut
import           Tests.Utils       hiding (exec)

import qualified Htcc.CRules.Types as CT

exec :: IO ()
exec = runTestsEx
    [ (StatementEqual.test "return 1+2;", 3)
    , (StatementEqual.test "return 1+2+4;", 7)
    , (StatementEqual.test "return 10-7+3;", 6)
    , (StatementEqual.test "return 42+23-30;", 35)
    , (StatementEqual.test "return 42/2+2-5;", 18)
    , (StatementEqual.test "return (3+5)/2;", 4)
    , (StatementEqual.test "return (4-2)*8+20/4;",21)
    , (StatementEqual.test "return -(-3*+5);", 15)
    , (StatementEqual.test "return -25+30;", 5)
    , (StatementEqual.test "return 42 == 42;", 1)
    , (StatementEqual.test "return 42 != 53;", 1)
    , (StatementEqual.test "return 42 < 53;", 1)
    , (StatementEqual.test "return 53 > 42;", 1)
    , (StatementEqual.test "return 42 <= 42;", 1)
    , (StatementEqual.test "return 32 <= 42;", 1)
    , (StatementEqual.test "return 42 >= 42;", 1)
    , (StatementEqual.test "return 53 >= 42;", 1)
    , (StatementEqual.test "return (1 + 1) == 2;", 1)
    , (StatementEqual.test "return (2 * 3) != 2;", 1)
    , (StatementEqual.test "return 1 || 0;", 1)
    , (StatementEqual.test "return (1 + 1) || 0 || 0;", 1)
    , (StatementEqual.test "return 0 || 0;", 0)
    , (StatementEqual.test "return 0 || (1 - 1);", 0)
    , (StatementEqual.test "return 2 || 1;", 1)
    , (StatementEqual.test "return 1 && 2;", 1)
    , (StatementEqual.test "return 2 && 3 && 4 && 0;", 0)
    , (StatementEqual.test "a = 1; return a;", 1)
    , (StatementEqual.test "a = 42; b = 20; return a + b;", 62)
    , (StatementEqual.test "a = 42; b = 20; c = 32; return (a - c) * b / 10;", 20)
    , (StatementEqual.test "a = 42; b = 20; return a - b;", 22)
    , (StatementEqual.test "a = 3; returnb = 5 * 6 - 8; return a + returnb / 2;", 14)
    , (StatementEqual.test "a = 3; return_ = 5 * 6 - 8; return a + return_ / 2;", 14)
    , (StatementEqual.test "a /* comment */ = 3; b = 5 */*comment*/ 6 - 8; return a + b / 2;", 14)
    , (StatementEqual.test "if (1) return 42; return 53;", 42)
    , (StatementEqual.test "if (20*3-60) return 42; return 53;", 53)
    , (StatementEqual.test "a = 1; b = 2; if (a) return b; return 42;", 2)
    , (StatementEqual.test "if (1) return 42; else return 53;", 42)
    , (StatementEqual.test "if (0) return 42; else return 53;", 53)
    , (StatementEqual.test "a = 0; b = 2; if (a) return b; else return b * 2;", 4)
    , (StatementEqual.test "a = 1; b = 0; if (b) return 42; if (0) return 42; else return a;", 1)
    , (StatementEqual.test "a = 1; b = 2; if (a) if (b) return b; else return 53; else return 24;", 2)
    , (StatementEqual.test "if (1) if (1) if (1) if (1) if (1) if (0) return 1; else return 2; else return 3; else return 4; else return 5; else return 6; else return 7;", 2)
    , (StatementEqual.test "if(1)if(1)return 42;return 53;", 42)
    , (StatementEqual.test "if(0); return 0;", 0)
    ]
{-
exec = let sizeof = CT.sizeof :: CT.TypeKind Integer -> Natural in runTestsEx [
    (StatementEqual.test "int main() { return 42; }", 42),
    (StatementEqual.test "int main() { return 1+2; }", 3),
    (StatementEqual.test "int main() { return 1+2+4; }", 7),
    (StatementEqual.test "int main() { return 10-7+3; }", 6),
    (StatementEqual.test "int main() { return 42+23-30; }", 35),
    (StatementEqual.test "int main() { return 42/2+2-5; }", 18),
    (StatementEqual.test "int main() { return (3+5)/2; }", 4),
    (StatementEqual.test "int main() { return (4-2)*8+20/4; }",21),
    (StatementEqual.test "int main() { return -(-3*+5); }", 15),
    (StatementEqual.test "int main() { return -25+30; }", 5),
    (StatementEqual.test "int main() { return 42 == 42; }", 1),
    (StatementEqual.test "int main() { return 42 != 53; }", 1),
    (StatementEqual.test "int main() { return 42 < 53; }", 1),
    (StatementEqual.test "int main() { return 53 > 42; }", 1),
    (StatementEqual.test "int main() { return 42 <= 42; }", 1),
    (StatementEqual.test "int main() { return 32 <= 42; }", 1),
    (StatementEqual.test "int main() { return 42 >= 42; }", 1),
    (StatementEqual.test "int main() { return 53 >= 42; }", 1),
    (StatementEqual.test "int main() { return (1 + 1) == 2; }", 1),
    (StatementEqual.test "int main() { return (2 * 3) != 2; }", 1),
    (StatementEqual.test "int main() { int a=42; int b=20; return a+b; }", 62),
    (StatementEqual.test "int main() { int a=42; int b=20; int c=32; return (a - c) * b / 10; }", 20),
    (StatementEqual.test "int main() { int hoge=42; int foo=20; return hoge - foo; }", 22),
    (StatementEqual.test "int main() { int returnx = 42; return returnx; return 53; }", 42),
    (StatementEqual.test "int main() { int a = 3; int b = 5 * 6 - 8; return a + b / 2; }", 14),
    (StatementEqual.test "int main() { if (1) return 42; return 53; }", 42),
    (StatementEqual.test "int main() { if (20*3-60) return 42; return 53; }", 53),
    (StatementEqual.test "int main() { int a = 1; int b = 2; if (a) return b; return 42; }", 2),
    (StatementEqual.test "int main() { if (1) return 42; else return 53; }", 42),
    (StatementEqual.test "int main() { if (0) return 42; else return 53; }", 53),
    (StatementEqual.test "int main() { int a = 0; int b = 2; if (a) return b; else return b * 2; }", 4),
    (StatementEqual.test "int main() { int a = 1; int b = 0; if (b) return 42; if (0) return 42; else return a; }", 1),
    (StatementEqual.test "int main() { int a = 1; int b = 2; if (a) if (b) return b; else return 53; else return 24; }", 2),
    (StatementEqual.test "int main() { if (1) if (1) if (1) if (1) if (1) if (0) return 1; else return 2; else return 3; else return 4; else return 5; else return 6; else return 7; }", 2),
    (StatementEqual.test "int main() { if(1)if(1)return 42;return 53; }", 42),
    (StatementEqual.test "int main() { if(0); return 0; }", 0),
    (StatementEqual.test "int main() { int a = 1; while (a < 10) a = a + 1; return a; }", 10),
    (StatementEqual.test "int main() { int a = 1; while (a < 10) a = a + 1; int b = 1; while (b < 20) b = b + 2; return a + b; }", 31),
    (StatementEqual.test "int main() { int a = 0; while (a); return 0; }", 0),
    (StatementEqual.test "int main() { int a = 0; int i = 0; for (i = 1; i <= 10; i = i + 1) a = a + i * 2; return a; }", 110),
    (StatementEqual.test "int main() { int i = 0; for (; i <= 10;) i = i + 2; return i; }", 12),
    (StatementEqual.test "int main() { int i = 0; for (; i <= 10; i = i + 2);  return i; }", 12),
    (StatementEqual.test "int main() { int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) if (a) a = 0; else a = 1; return a; }", 0),
    (StatementEqual.test "int main() { { int a = 42; int b = 2; return a / b; } }", 21),
    (StatementEqual.test "int main() { int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) { a = a + i; a = a - i; } return a; }", 0),
    (StatementEqual.test "int main() { int a = 10; if (a) { a = a * a; a = a / 10; } return a; }", 10),
    (StatementEqual.test "int main() { int a = 0; while (1) { if (a < 10) a = a + 1; else return a; } }", 10),
    (StatementEqual.test "int main() { int a = 0; for (;;) { a = 42; return a; } return a; }", 42),
    (StatementEqual.test "int main() { int a = 0; for (;;) { if (a < 10) a = a + 1; else return a; } }", 10),
    (LinkFuncRet.test "int test_func1(); int main() { int a = test_func1(); test_func1(); return a; }" ["test_func1"], 0),
    (StatementEqual.test "int main() { int a = 1; int b = 1; return a & b; }", 1),
    (StatementEqual.test "int main() { int a = 42; int b = 53; a = a ^ b; b = b ^ a; a = a ^ b; if (a == 53) if (b == 42) return 1; return 0; }", 1),
    (StatementEqual.test "int main() { return 1 | 0; }", 1),
    (StatementEqual.test "int main() { int a = 1; int b = 0; return a & b ^ a | b; }", 1), -- Xor swap
    (StatementEqual.test "int main() { int a = 0; int i = 0; for (i = 0; i < 10; i = i + 1) if (i % 2 == 0) a = a + i; return a; }", 20),
    (StatementEqual.test "int main() { return !0; }", 1),
    (StatementEqual.test "int main() { return !42; }", 0),
    (StatementEqual.test "int main() { return !!!0; }", 1),
    (StatementEqual.test "int main() { return ~(-42); }", 41),
    (StatementEqual.test "int main() { return ~~~~42; }", 42),
    (StatementEqual.test "int main() { return (2 * 4) == (2 << 2); }", 1),
    (StatementEqual.test "int main() { return (8 / 4) == (8 >> 2); }", 1),
    (StatementEqual.test "int main() { int a = 2 << 4; return (a & (a - 1)) == 0; }", 1), -- Determining if an integer is a power of 2
    (StatementEqual.test "int main() { 1; {2;} return 3; }", 3),
    -- (LinkFuncRet.test "int main() { return sum7(1, 1, 1, 1, 1, 1, 1); }" ["test_func3"], 7),
    (LinkFuncRet.test "int test_func2(); int main() { return test_func2(40); }" ["test_func2"], 0),
    -- (LinkFuncRet.test "int main() { return test_func2(sum7(1, 2, 3, 4, 5, 6, 7)); }" ["test_func2", "test_func3"], 0),
    -- (LinkFuncRet.test "int main() { return sum16(1,1,1,1,1,1,11,10,9,8,7,6,5,4,3,2); }" ["test_func3"], 11),
    (StatementEqual.test "int f() { return 42; } int main() { return f(); }", 42),
    (StatementEqual.test "int g() { return 42; } int f() { return g(); } int main() { return f(); }", 42),
    (StatementEqual.test "int id(int a) { return a; } int main() { int a = 1; return id(a-1) + id(1); }", 1),
    (StatementEqual.test "int get1() { return 1; } int get2() { return 2; } int main() { int a = get1(); return a + get2(); }", 3),
    (StatementEqual.test "int add(int a, int b) { return a + b; } int main() { return add(1, 2); }", 3),
    (StatementEqual.test "int rec(int a) { if (a == 0) return 42; return rec(a - 1); } int main() { int b = rec(2); return 1 + 2; }", 3),
    (StatementEqual.test "int fib(int n) { if (n == 0) return 1; else if (n == 1) return 1; else if (n >= 2) return fib(n - 1) + fib(n - 2); else return 0; } int main() { return fib(5); }", 8), -- fibonacci number
    (StatementEqual.test "int main() { int a = 42; int* b = &a; return *b; }", 42),
    (StatementEqual.test "int main() { int a = 42; return *&a; }", 42),
    (StatementEqual.test "int main() { int a = 42; int* b = &a; int** c = &b; return **c; }", 42),
    (StatementEqual.test "int main() { int a = 42; int* b = &a; *b = a * 2; return a; }", 84),
    (StatementEqual.test "int main() { int a = 42; int b = 5; return *(&b+1); }", 42),
    (StatementEqual.test "int main() { int a = 42; int b = 5; *(&a-1) = 53; return b; }", 53),
    (StatementEqual.test "int main() { int a = 42; int b = 5; *(&b+1) = 53; return a; }", 53),
    (StatementEqual.test "int main() { int sum = 0; int i = 1; for (; i < 4; i = i + 1) sum = sum + i; return sum; }", 6),
    (StatementEqual.test "int main() { int ar[2]; int* p = ar; *p = 3; return *ar; }", 3),
    (StatementEqual.test "int main() { int ar[2]; int* p = ar; *(p + 1) = 3; return *(ar + 1); }", 3),
    (StatementEqual.test "int main() { int ar[2]; int* p = ar; *p = 2; *(p + 1) = 3; return *ar + *(ar + 1); }", 5),
    (StatementEqual.test "int main() { int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; return *ar; }", 1),
    (StatementEqual.test "int main() { int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; return *(ar + 1); }", 2),
    (StatementEqual.test "int main() { int ar[3]; *ar = 1; *(ar + 1) = 2; *(ar + 2) = 3; return *(ar + 2); }", 3),
    (StatementEqual.test "int f(int* p) { *p = 42; return 0; } int main() { int a = 0; f(&a); return a; }", 42),
    (StatementEqual.test "int main() { int ar[10]; int i = 0; for (; i < 10; i = i + 1) { *(ar + i) = i; } int sum = 0; for (i = 0; i < 10; i = i + 1) { sum = sum + *(ar + i); } return sum; }", 45),
    (StatementEqual.test "int sum(int* p, int n) { int sum = 0; int i = 0; for (; i < n; i = i + 1) sum = sum + *(p + i); return sum; } int main() { int ar[10]; int i = 0; for (; i < 10; i = i + 1) *(ar + i) = i; return sum(ar, 10); }", 45),
    (StatementEqual.test "int main() { int ar[2][3]; int sum = 0; int i = 0; for (; i < 2; i = i + 1) { int j = 0; for (; j < 3; j = j + 1) { *(*(ar + i) + j) = i + j; sum = sum + *(*(ar + i) + j); } } return sum; } ", 9),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; *p = 42; return **ar; }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; *(p + 1) = 42; return *(*ar + 1); }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; *(p + 2) = 42; return *(*ar + 2); }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; *(p + 3) = 42; return **(ar + 1); }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; *(p + 4) = 42; return *(*(ar + 1) + 1); }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; *(p + 5) = 42; return *(*(ar + 1) + 2); }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; *(p + 6) = 42; return **(ar + 2); }", 42),
    (StatementEqual.test "int main() { int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; return ar[0]; }", 0),
    (StatementEqual.test "int main() { int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; return ar[1]; }", 1),
    (StatementEqual.test "int main() { int ar[3]; int i = 0; for (; i < 3; i = i + 1) ar[i] = i; return ar[2]; }", 2),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; p[0] = 42; return ar[0][0]; }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; p[1] = 42; return ar[0][1]; }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; p[2] = 42; return ar[0][2]; }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; p[3] = 42; return ar[1][0]; }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; p[4] = 42; return ar[1][1]; }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; p[5] = 42; return ar[1][2]; }", 42),
    (StatementEqual.test "int main() { int ar[2][3]; int* p = ar; p[6] = 42; return ar[2][0]; }", 42),
    (StatementEqual.test "int main() { int a; return sizeof(a); }", fromIntegral $ sizeof CT.CTInt),
    (StatementEqual.test "int main() { int a; return sizeof a; }", fromIntegral $ sizeof CT.CTInt),
    (StatementEqual.test "int main() { int* p; return sizeof p; }", fromIntegral $ sizeof $ CT.CTPtr CT.CTInt),
    (StatementEqual.test "int main() { int ar[3]; return sizeof ar; }", fromIntegral $ sizeof $ CT.CTArray 3 CT.CTInt),
    (StatementEqual.test "int main() { int ar[3][5]; return sizeof ar; }", fromIntegral $ sizeof $ CT.CTArray 5 $ CT.CTArray 3 CT.CTInt),
    (StatementEqual.test "int main() { int ar[3][5]; return sizeof *ar; }", fromIntegral $ sizeof $ CT.CTArray 5 CT.CTInt),
    (StatementEqual.test "int main() { int ar[3][5]; return sizeof **ar; }", fromIntegral $ sizeof CT.CTInt),
    (StatementEqual.test "int main() { int ar[3][5]; return sizeof(**ar) + 1; }", succ $ fromIntegral $ sizeof CT.CTInt),
    (StatementEqual.test "int main() { int ar[3][5]; return sizeof **ar + 1; }", succ $ fromIntegral $ sizeof CT.CTInt),
    (StatementEqual.test "int main() { int ar[3][5]; return sizeof(**ar + 1); }", fromIntegral $ sizeof $ CT.CTLong CT.CTInt),
    (StatementEqual.test "int main() { int ar[2]; 2[ar] = 42; return ar[2]; }", 42),
    (StatementEqual.test "int g; int main() { return g; }", 0),
    (StatementEqual.test "int g; int main() { g = 42; return g; }", 42),
    (StatementEqual.test "int gr[3]; int main() { int i = 0; for (; i < sizeof gr / sizeof gr[0]; i = i + 1) gr[i] = i + 1; return gr[0]; }", 1),
    (StatementEqual.test "int gr[3]; int main() { int i = 0; for (; i < sizeof gr / sizeof gr[0]; i = i + 1) gr[i] = i + 1; return gr[1]; }", 2),
    (StatementEqual.test "int gr[3]; int main() { int i = 0; for (; i < sizeof gr / sizeof gr[0]; i = i + 1) gr[i] = i + 1; return gr[2]; }", 3),
    (StatementEqual.test "int main() { char c = 1; return c; }", 1),
    (StatementEqual.test "int main() { char c1 = 1; char c2 = 2; return c1; }", 1),
    (StatementEqual.test "int main() { char c1 = 1; char c2 = 2; return c2; }", 2),
    (StatementEqual.test "int main() { char x; return sizeof x; }", 1),
    (StatementEqual.test "int main() { char ar[10]; return sizeof ar; }", fromIntegral $ sizeof $ CT.CTArray 10 CT.CTChar),
    (StatementEqual.test "int f(char a, char b, char c) { return a - b - c; } int main() { return f(7, 3, 3); }", 1),
    (StatementEqual.test "int f(char a, int b, char c) { return a - b - c; } int main() { return f(7, 3, 3); }", 1),
    (StatementEqual.test "int main() { return \"abc\"[0]; }", ord 'a'),
    (StatementEqual.test "int main() { return \"abc\"[1]; }", ord 'b'),
    (StatementEqual.test "int main() { return \"abc\"[2]; }", ord 'c'),
    (StatementEqual.test "int main() { return \"abc\"[3]; }", 0),
    (StatementEqual.test "int main() { char* p = \"abc\"; return p[2]; }", ord 'c'),
    (StatementEqual.test "int main() { return sizeof \"abc\"; }", 4),
    (StatementEqual.test "int main() { return \"\\a\"[0]; }", ord '\a'),
    (StatementEqual.test "int main() { return \"\\b\"[0]; }", ord '\b'),
    (StatementEqual.test "int main() { return \"\\t\"[0]; }", ord '\t'),
    (StatementEqual.test "int main() { return \"\\n\"[0]; }", ord '\n'),
    (StatementEqual.test "int main() { return \"\\v\"[0]; }", ord '\v'),
    (StatementEqual.test "int main() { return \"\\f\"[0]; }", ord '\f'),
    (StatementEqual.test "int main() { return \"\\r\"[0]; }", ord '\r'),
    (StatementEqual.test "int main() { return \"\\e\"[0]; }", ord '\ESC'),
    (StatementEqual.test "int main() { return \"\\\\0\"[0]; }", ord '\0'),
    (StatementEqual.test "int main() { return \"\\j\"[0]; }", ord 'j'),
    (StatementEqual.test "int main() { return \"\\k\"[0]; }", ord 'k'),
    (StatementEqual.test "int main() { return \"\\l\"[0]; }", ord 'l'),
    (StatementEqual.test "int main() { return ({ 42; }); }", 42),
    (StatementEqual.test "int main() { return ({ 1; 2; 3; }); }", 3),
    (StatementEqual.test "int main() { ({ 1; return 2; 3; }); return 4; }", 2),
    (StatementEqual.test "int main() { return ({ int a = 42; a; }); }", 42),
    (StatementEqual.test "int main() { /* return 0; */ return 42; }", 42),
    (StatementEqual.test "int main() { // hoge\nreturn 42; }", 42),
    (StatementEqual.test "int main() { int a = 42; { int a = 32; } return a; }", 42),
    (StatementEqual.test "int main() { int a = 42; { int a = 32; } { int a = 53; return a; } return 42; }", 53),
    (StatementEqual.test "int main() { int a = 42; { a = 32; } return a; }", 32),
    (StatementEqual.test "int main() { int* ar[3]; int x; ar[0] = &x; x = 42; ar[0][0]; }", 42)
    ] >> runTestsEx [
    (LinkFuncStdOut.test "int test_func1(); int main() { return test_func1(); }" ["test_func1"], Right "test/Tests/csrc/test_func1.c::test_func1(): [OK]"),
    (LinkFuncStdOut.test "int test_func2(); int main() { return test_func2(40); }" ["test_func2"], Right "test/Tests/csrc/test_func2.c::test_func2(40) outputs: \"2 3 5 7 11 13 17 19 23 29 31 37 \": [OK]"),
    ]
-}
