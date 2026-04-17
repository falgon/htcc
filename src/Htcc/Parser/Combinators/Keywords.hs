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
    kLong, kRegister, kRestrict, kReturn, kShort, kSigned, kSizeof,
    kStatic, kStruct, kSwitch, kTypedef, kUnion, kUnsigned, kVoid,
    kVolatile, kWhile, kAlignas, kAlignof, kAtomic, kBool, kComplex,
    kGeneric, kImaginary, kNoreturn, kStaticAssert, kThreadLocal,
    kBasicTypes
) where

import qualified Data.Text                    as T
import qualified Htcc.CRules                  as CR
import           Htcc.Parser.Combinators.Core
import qualified Text.Megaparsec              as M
import qualified Text.Megaparsec.Char         as MC

pKeyword :: (Monad m, Ord e) => T.Text -> M.ParsecT e T.Text m T.Text
pKeyword = flip notFollowedBy (M.takeWhile1P (Just "valid Keyword") CR.isValidChar) . MC.string

kAuto, kBreak, kCase, kChar, kConst, kContinue,
    kDefault, kDo, kDouble, kElse, kEnum, kExtern,
    kFloat, kFor, kGoto, kIf, kInline, kInt,
    kLong, kRegister, kRestrict, kReturn, kShort, kSigned,
    kSizeof, kStatic, kStruct, kSwitch, kTypedef, kUnion,
    kUnsigned, kVoid, kVolatile, kWhile, kAlignas, kAlignof,
    kAtomic, kBool, kComplex, kGeneric, kImaginary, kNoreturn,
    kStaticAssert, kThreadLocal :: (Monad m, Ord e) => M.ParsecT e T.Text m T.Text
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
kAlignas = pKeyword "_Alignas"
kAlignof = pKeyword "_Alignof"
kAtomic = pKeyword "_Atomic"
kBool = pKeyword "_Bool"
kComplex = pKeyword "_Complex"
kGeneric = pKeyword "_Generic"
kImaginary = pKeyword "_Imaginary"
kNoreturn = pKeyword "_Noreturn"
kStaticAssert = pKeyword "_Static_assert"
kThreadLocal = pKeyword "_Thread_local"

kBasicTypes :: (Monad m, Ord e) => [M.ParsecT e T.Text m T.Text]
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
  , kBool
  , kComplex
  , kImaginary
  ]
