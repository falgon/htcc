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
import qualified Htcc.CRules                  as CR
import           Htcc.Parser.Combinators.Core
import qualified Text.Megaparsec              as M
import qualified Text.Megaparsec.Char         as MC

pKeyword :: Ord e => T.Text -> M.ParsecT e T.Text m T.Text
pKeyword = flip notFollowedBy (M.takeWhile1P (Just "valid Keyword") CR.isValidChar) . MC.string

kAuto, kBreak, kCase, kChar, kConst, kContinue,
    kDefault, kDo, kDouble, kElse, kEnum, kExtern,
    kFloat, kFor, kGoto, kIf, kInline, kInt,
    kLong, kRegister, kRestrict, kReturn, kShort, kSigned,
    kSizeof, kStatic, kStruct, kSwitch, kTypedef, kUnion,
    kUnsigned, kVoid, kVolatile, kWhile, k_Alignas, k_Alignof,
    k_Atomic, k_Bool, k_Complex, k_Generic, k_Imaginary, k_Noreturn,
    k_Static_assert, k_Thread_local :: Ord e => M.ParsecT e T.Text m T.Text
kAuto = pKeyword "auto"
kBreak = pKeyword "break"
kCase = pKeyword "case"
kChar = pKeyword "char"
kConst = pKeyword "const"
kContinue = pKeyword "continue"
kDefault = pKeyword "default"
kDo = pKeyword "do"
kDouble = pKeyword "double"
kElse = pKeyword "else"
kEnum = pKeyword "enum"
kExtern = pKeyword "extern"
kFloat = pKeyword "float"
kFor = pKeyword "for"
kGoto = pKeyword "goto"
kIf = pKeyword "if"
kInline = pKeyword "inline"
kInt = pKeyword "int"
kLong = pKeyword "long"
kRegister = pKeyword "register"
kRestrict = pKeyword "restrict"
kReturn = pKeyword "return"
kShort = pKeyword "short"
kSigned = pKeyword "signed"
kSizeof = pKeyword "sizeof"
kStatic = pKeyword "static"
kStruct = pKeyword "struct"
kSwitch = pKeyword "switch"
kTypedef = pKeyword "typedef"
kUnion = pKeyword "union"
kUnsigned = pKeyword "unsigned"
kVoid = pKeyword "void"
kVolatile = pKeyword "volatile"
kWhile = pKeyword "while"
k_Alignas = pKeyword "_Alignas"
k_Alignof = pKeyword "_Alignof"
k_Atomic = pKeyword "_Atomic"
k_Bool = pKeyword "_Bool"
k_Complex = pKeyword "_Complex"
k_Generic = pKeyword "_Generic"
k_Imaginary = pKeyword "_Imaginary"
k_Noreturn = pKeyword "_Noreturn"
k_Static_assert = pKeyword "_Static_assert"
k_Thread_local = pKeyword "_Thread_local"

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

