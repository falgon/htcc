{-|
Module      : Htcc.Asm.Intrinsic.Instruction
Description : Types, classes and function of the x86_64 instructions
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

`Htcc.Asm.Intrinsic.Instruction` exports types, classes and function of the x86_64 instructions.
-}
{-# LANGUAGE OverloadedStrings #-}

module Htcc.Asm.Intrinsic.Instruction (
    -- * Instructions
    UnaryInstruction (..),
    BinaryInstruction (..),
    sete,
    setne,
    setl,
    setle,
    setg,
    setge,
    cqo,
    ret,
    jmp,
    je,
    call
) where

import qualified Data.Text as T

import Htcc.Asm.Intrinsic.Register (Register (..))
import Htcc.Asm.Intrinsic.Operand (IsOperand (..), Ref (..))
import Htcc.Utils (tshow)

{-# INLINE intelSyntaxUnaryInst #-}
intelSyntaxUnaryInst :: Show a => T.Text -> a -> T.Text
intelSyntaxUnaryInst = flip (.) (T.append " " . flip T.append "\n" . tshow) . (.) (T.append "\t") . T.append

{-# INLINE intelSyntaxBinaryInst #-}
intelSyntaxBinaryInst :: (Show a, Show b) => T.Text -> a -> b -> T.Text
intelSyntaxBinaryInst = (.) (flip (.) (T.append ", " . flip T.append "\n" . tshow) . (.) (T.append "\t")) . (flip (.) ((.) (T.append " ") . (T.append . tshow)) . (.) . T.append)

-- | A class of x86_64 instructions with unary arguments.
class Show a => UnaryInstruction a where
    -- | THe push instruction.
    push :: a -> T.Text
    push = intelSyntaxUnaryInst "push"
    -- | The pop instruction.
    pop :: a -> T.Text
    pop = intelSyntaxUnaryInst "pop"
    -- | The pushl instruction.
    pushl :: a -> T.Text
    pushl = intelSyntaxUnaryInst "pushl"
    -- | The popl instruction.
    popl :: a -> T.Text
    popl = intelSyntaxUnaryInst "popl"
    -- | The idiv instruction.
    idiv :: a -> T.Text
    idiv = intelSyntaxUnaryInst "idiv"
    -- | The not instructions.
    not :: a -> T.Text
    not = intelSyntaxUnaryInst "not"

-- | The sete instruction.
sete :: Register -> T.Text
sete = intelSyntaxUnaryInst "sete"

-- | The setne instruction.
setne :: Register -> T.Text
setne = intelSyntaxUnaryInst "setne"

-- | The setl instruction.
setl :: Register -> T.Text
setl = intelSyntaxUnaryInst "setl"

-- | The setle instruction.
setle :: Register -> T.Text
setle = intelSyntaxUnaryInst "setle"

-- | The setg instruction.
setg :: Register -> T.Text
setg = intelSyntaxUnaryInst "setg"

-- | The setge instruction.
setge :: Register -> T.Text
setge = intelSyntaxUnaryInst "setge"

instance UnaryInstruction Integer
instance UnaryInstruction Int
instance UnaryInstruction Register

-- | A class of x86_64 instructions with binary arguments.
class Show a => BinaryInstruction a where
    -- | The mov instruction.
    mov :: BinaryInstruction b => a -> b -> T.Text
    mov = intelSyntaxBinaryInst "mov"
    -- | The movl instruction.
    movl :: BinaryInstruction b => a -> b -> T.Text
    movl = intelSyntaxBinaryInst "movl"
    -- | The cmp instruction.
    cmp :: BinaryInstruction b => a -> b -> T.Text
    cmp = intelSyntaxBinaryInst "cmp"
    -- | The add instruction.
    add :: BinaryInstruction b => a -> b -> T.Text
    add = intelSyntaxBinaryInst "add"
    -- | The sub instruction.
    sub :: BinaryInstruction b => a -> b -> T.Text
    sub = intelSyntaxBinaryInst "sub"
    -- | The imul instruction.
    imul :: BinaryInstruction b => a -> b -> T.Text
    imul = intelSyntaxBinaryInst "imul"
    -- | The and instruction.
    and :: BinaryInstruction b => a -> b -> T.Text
    and = intelSyntaxBinaryInst "and"
    -- | The or instruction.
    or :: BinaryInstruction b => a -> b -> T.Text
    or = intelSyntaxBinaryInst "or"
    -- | The xor instruction.
    xor :: BinaryInstruction b => a -> b -> T.Text
    xor = intelSyntaxBinaryInst "xor"
    -- | The shl instruction.
    shl :: BinaryInstruction b => a -> b -> T.Text
    shl = intelSyntaxBinaryInst "shl"
    -- | The sar instruction.
    sar :: BinaryInstruction b => a -> b -> T.Text
    sar = intelSyntaxBinaryInst "sar"
    -- | The movzb instruction.
    movzb :: a -> Register -> T.Text
    movzb = intelSyntaxBinaryInst "movzb"

instance BinaryInstruction Integer
instance BinaryInstruction Int
instance BinaryInstruction Register
instance IsOperand a => BinaryInstruction (Ref a)

-- | The cqo instruction.
cqo :: T.Text
cqo = "\tcqo\n"

-- | The ret instruction.
ret :: T.Text
ret = "\tret\n"

-- | The jmp instruction.
jmp :: T.Text -> T.Text
jmp = flip T.append "\n" . T.append "\tjmp "

-- | The je instruction.
je :: T.Text -> T.Text
je = flip T.append "\n" . T.append "\tje "

-- | The call instruction.
call :: T.Text -> T.Text
call = flip T.append "\n" . T.append "\tcall "

