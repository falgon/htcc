{-|
Module      : Htcc.Parser.Combinators.Keywords
Description : C language lexer
Copyright   : (c) roki, 2020~
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

C language lexer
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Parser.Combinators.Keywords (
    kAuto, kBreak, kCase, kChar, kConst, kContinue,
    kDefault, kDo, kDouble, kElse, kEnum, kExtern,
    kFloat, kFor, kGoto, kIf, kInline, kInt,
    kLong, kRegister, kRestrict, kReturn, kShort, kSigned,
    kSizeof, kStatic, kStruct, kSwitch, kTypedef, kUnion,
    kUnsigned, kVoid, kVolatile, kWhile, k_Alignas, k_Alignof,
    k_Atomic, k_Bool, k_Complex, k_Generic, k_Imaginary, k_Noreturn,
    k_Static_assert, k_Thread_local, kBasicTypes
) where

import qualified Data.Text                    as T
import           Htcc.Parser.Combinators.Core
import qualified Text.Megaparsec as M

kAuto, kBreak, kCase, kChar, kConst, kContinue,
    kDefault, kDo, kDouble, kElse, kEnum, kExtern,
    kFloat, kFor, kGoto, kIf, kInline, kInt,
    kLong, kRegister, kRestrict, kReturn, kShort, kSigned,
    kSizeof, kStatic, kStruct, kSwitch, kTypedef, kUnion,
    kUnsigned, kVoid, kVolatile, kWhile, k_Alignas, k_Alignof,
    k_Atomic, k_Bool, k_Complex, k_Generic, k_Imaginary, k_Noreturn,
    k_Static_assert, k_Thread_local :: Ord e => M.ParsecT e T.Text m T.Text
kAuto = symbol "auto"
kBreak = symbol "break"
kCase = symbol "case"
kChar = symbol "char"
kConst = symbol "const"
kContinue = symbol "continue"
kDefault = symbol "default"
kDo = symbol "do"
kDouble = symbol "double"
kElse = symbol "else"
kEnum = symbol "enum"
kExtern = symbol "extern"
kFloat = symbol "float"
kFor = symbol "for"
kGoto = symbol "goto"
kIf = symbol "if"
kInline = symbol "inline"
kInt = symbol "int"
kLong = symbol "long"
kRegister = symbol "register"
kRestrict = "restrict"
kReturn = symbol "return"
kShort = symbol "short"
kSigned = symbol "signed"
kSizeof = symbol "sizeof"
kStatic = symbol "static"
kStruct = symbol "struct"
kSwitch = symbol "switch"
kTypedef = symbol "typedef"
kUnion = symbol "union"
kUnsigned = symbol "unsigned"
kVoid = symbol "void"
kVolatile = symbol "volatile"
kWhile = symbol "while"
k_Alignas = symbol "_Alignas"
k_Alignof = symbol "_Alignof"
k_Atomic = symbol "_Atomic"
k_Bool = symbol "_Bool"
k_Complex = symbol "_Complex"
k_Generic = symbol "_Generic"
k_Imaginary = symbol "_Imaginary"
k_Noreturn = symbol "_Noreturn"
k_Static_assert = symbol "_Static_assert"
k_Thread_local = symbol "_Thread_local"

kBasicTypes :: Ord e => [M.ParsecT e T.Text m T.Text]
kBasicTypes = [
    kChar
  , kDouble
  , kFloat
  , kInt
  , kLong
  , kShort
  , kSigned
  , kUnsigned
  , kVoid
  , k_Bool
  , k_Complex
  , k_Imaginary
  ]

