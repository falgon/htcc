{-# LANGUAGE OverloadedStrings #-}
module Main where

import Tests.Utils
import qualified Tests.Test1 as StatementEqual
import qualified Tests.Test2 as LinkFuncRet
import qualified Tests.Test3 as LinkFuncStdOut

main :: IO ()
main = runTestsEx [
    (StatementEqual.test "42;", 42),
    (StatementEqual.test "1+2;", 3),
    (StatementEqual.test "1+2+4;", 7),
    (StatementEqual.test "10-7+3;", 6),
    (StatementEqual.test "42+23-30;", 35),
    (StatementEqual.test "42/2+2-5;", 18),
    (StatementEqual.test "(3+5)/2;", 4),
    (StatementEqual.test "(4-2)*8+20/4;",21),
    (StatementEqual.test "-(-3*+5);", 15),
    (StatementEqual.test "-25+30;", 5),
    (StatementEqual.test "42 == 42;", 1),
    (StatementEqual.test "42 != 53;", 1),
    (StatementEqual.test "42 < 53;", 1),
    (StatementEqual.test "53 > 42;", 1),
    (StatementEqual.test "42 <= 42;", 1),
    (StatementEqual.test "32 <= 42;", 1),
    (StatementEqual.test "42 >= 42;", 1),
    (StatementEqual.test "53 >= 42;", 1),
    (StatementEqual.test "(1 + 1) == 2;", 1),
    (StatementEqual.test "(2 * 3) != 2;", 1),
    (StatementEqual.test "a=42; b=20; a+b;", 62),
    (StatementEqual.test "a=42; b=20; c=32; (a - c) * b / 10;", 20),
    (StatementEqual.test "hoge=42; foo=20; hoge - foo;", 22),
    (StatementEqual.test "hoge=42; return hoge;", 42),
    (StatementEqual.test "returnx = 42; return returnx; return 53;", 42),
    (StatementEqual.test "a = 3; b = 5 * 6 - 8; return a + b / 2;", 14),
    (StatementEqual.test "if (1) return 42; return 53;", 42),
    (StatementEqual.test "if (20*3-60) return 42; return 53;", 53),
    (StatementEqual.test "a = 1; b = 2; if (a) return b; return 42;", 2),
    (StatementEqual.test "if (1) return 42; else return 53;", 42),
    (StatementEqual.test "if (0) return 42; else return 53;", 53),
    (StatementEqual.test "a = 0; b = 2; if (a) return b; else return b * 2;", 4),
    (StatementEqual.test "a = 1; b = 0; if (b) return 42; if (0) return 42; else return a;", 1),
    (StatementEqual.test "a = 1; b = 2; if (a) if (b) return b; else return 53; else return 24;", 2),
    (StatementEqual.test "if (1) if (1) if (1) if (1) if (1) if (0) return 1; else return 2; else return 3; else return 4; else return 5; else return 6; else return 7;", 2),
    (StatementEqual.test "if(1)if(1)return 42;return 53;", 42),
    (StatementEqual.test "if(0); return 0;", 0),
    (StatementEqual.test "a = 1; while (a < 10) a = a + 1; return a;", 10),
    (StatementEqual.test "a = 1; while (a < 10) a = a + 1; b = 1; while (b < 20) b = b + 2; return a + b;", 31),
    (StatementEqual.test "a = 0; while (a); return 0;", 0),
    
    (StatementEqual.test "a = 0; for (i = 1; i <= 10; i = i + 1) a = a + i * 2; return a;", 110),
    
    (StatementEqual.test "i = 0; for (; i <= 10;) i = i + 2; return i;", 12),
    (StatementEqual.test "i = 0; for (; i <= 10; i = i + 2);  return i;", 12),
    (StatementEqual.test "a = 0; for (i = 0; i < 10; i = i + 1) if (a) a = 0; else a = 1; return a;", 0),
    (StatementEqual.test "{ a = 42; b = 2; return a / b; }", 21),
    (StatementEqual.test "a = 0; for (i = 0; i < 10; i = i + 1) { a = a + i; a = a - i; } return a;", 0),
    (StatementEqual.test "a = 10; if (a) { a = a * a; a = a / 10; } return a;", 10),
    (StatementEqual.test "a = 0; while (1) { if (a < 10) a = a + 1; else return a; }", 10),
    (StatementEqual.test "a = 0; for (;;) { a = 42; return a; } return a;", 42),
    (StatementEqual.test "a = 0; for (;;) { if (a < 10) a = a + 1; else return a; }", 10),
    (LinkFuncRet.test "a = test_func1(); test_func1(); return a;" ["test_func1"], 0),
    (StatementEqual.test "a = 1; b = 1; return a & b;", 1),
    (StatementEqual.test "a = 42; b = 53; a = a ^ b; b = b ^ a; a = a ^ b; if (a == 53) if (b == 42) return 1; return 0;", 1),
    (StatementEqual.test "return 1 | 0;", 1),
    (StatementEqual.test "a = 1; b = 0; return a & b ^ a | b;", 1), -- Xor swap
    (StatementEqual.test "a = 0; for (i = 0; i < 10; i = i + 1) if (i % 2 == 0) a = a + i; return a;", 20),
    (StatementEqual.test "return !0;", 1),
    (StatementEqual.test "return !42;", 0),
    (StatementEqual.test "return !!!0;", 1),
    (StatementEqual.test "return ~(-42);", 41),
    (StatementEqual.test "return ~~~~42;", 42),
    (StatementEqual.test "return (2 * 4) == (2 << 2);", 1),
    (StatementEqual.test "return (8 / 4) == (8 >> 2);", 1),
    (StatementEqual.test "a = 2 << 4; return (a & (a - 1)) == 0;", 1), -- Determining if an integer is a power of 2,
    (LinkFuncRet.test "return sum7(1, 1, 1, 1, 1, 1, 1);" ["test_func3"], 7),
    (LinkFuncRet.test "return test_func2(40);" ["test_func2"], 0),
    (LinkFuncRet.test "return test_func2(sum7(1, 2, 3, 4, 5, 6, 7));" ["test_func2", "test_func3"], 0),
    (LinkFuncRet.test "return sum16(1,1,1,1,1,1,11,10,9,8,7,6,5,4,3,2);" ["test_func3"], 11)
    ] >> runTestsEx [
    (LinkFuncStdOut.test "return test_func1();" ["test_func1"], Right "test/Tests/csrc/test_func1.c::test_func1(): [OK]"),
    (LinkFuncStdOut.test "return test_func2(40);" ["test_func2"], Right "test/Tests/csrc/test_func2.c::test_func2(40) outputs: \"2 3 5 7 11 13 17 19 23 29 31 37 \": [OK]"),
    (LinkFuncStdOut.test "return test_func2(sum7(1, 2, 3, 4, 5, 6, 7));" ["test_func2", "test_func3"], Right "test/Tests/csrc/test_func2.c::test_func2(28) outputs: \"2 3 5 7 11 13 17 19 23 \": [OK]")
    ]
