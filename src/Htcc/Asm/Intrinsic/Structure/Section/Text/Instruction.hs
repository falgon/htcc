{-|
Module      : Htcc.Asm.Intrinsic.Structure.Section.Text.Instruction
Description : The modules of intrinsic (x86_64) assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The modules of intrinsic (x86_64) assembly
-}
{-# LANGUAGE OverloadedStrings #-}
module Htcc.Asm.Intrinsic.Structure.Section.Text.Instruction (
    SizeUnit (..),
    UnaryInstruction (..),
    BinaryInstruction (..),
    Offset (..),
    Ptr (..),
    sete, setne, setl, setle, setg, setge,
    byte, word, dword,
    cqo, ret, leave, 
    jmp, je, jne, jnz, 
    call
) where

import qualified Data.Text as T
import Numeric.Natural

import Htcc.Asm.Intrinsic.Register (Register (..))
import Htcc.Asm.Intrinsic.Operand (IsOperand (..), Ref (..))
import qualified Htcc.Asm.Intrinsic.Structure.Internal as I
import Htcc.Asm.Intrinsic.Structure.Section.Text.Directive
import Htcc.Utils (tshow)

{-# INLINE intelSyntaxUnary #-}
intelSyntaxUnary :: Show a => T.Text -> a -> I.Asm TextLabelCtx e ()
intelSyntaxUnary inst arg = I.putStrLnWithIndent $ inst <> " " <> tshow arg

{-# INLINE intelSyntaxBinary #-}
intelSyntaxBinary :: (Show a, Show b) => T.Text -> a -> b -> I.Asm TextLabelCtx e ()
intelSyntaxBinary inst lhs rhs = I.putStrLnWithIndent $ inst <> " " <> tshow lhs <> ", " <> tshow rhs

-- | Unit of size of data to be loaded
data SizeUnit = Byte -- ^ 8 bits
    | Word -- ^ 16 bits
    | DWord -- ^ 32 bits
    deriving (Eq, Ord, Enum, Bounded)

instance Show SizeUnit where
    show Byte = "byte"
    show Word = "word"
    show DWord = "dword"

-- | @offset@ instruction
newtype Offset = Offset T.Text -- ^ The constructor of @offset@ instruction

instance Show Offset where
    show (Offset s) = "offset " ++ T.unpack s

-- | The @ptr@ instruction
data Ptr a = Ptr SizeUnit (Ref a) -- ^ The constructor of @ptr@ instruction

instance IsOperand a => Show (Ptr a) where
    show (Ptr u s) = show u ++ " ptr " ++ show s

-- | @byte@ is a helper function for intuitively writing @byte@ instructions
byte :: IsOperand a => (SizeUnit -> Ref a -> Ptr a) -> Ref a -> Ptr a
byte = flip id Byte

-- | @word@ is a helper function for intuitively writing @word@ instructions
word :: IsOperand a => (SizeUnit -> Ref a -> Ptr a) -> Ref a -> Ptr a
word = flip id Word

-- | @dword@ is a helper function for intuitively writing @dword@ instructions
dword :: IsOperand a => (SizeUnit -> Ref a -> Ptr a) -> Ref a -> Ptr a
dword = flip id DWord

-- | A class of x86_64 instructions with unary arguments.
class Show a => UnaryInstruction a where
    -- | @push@ instruction
    push :: a -> I.Asm TextLabelCtx e ()
    push = intelSyntaxUnary "push"
    -- | @pop@ instruction
    pop :: a -> I.Asm TextLabelCtx e ()
    pop = intelSyntaxUnary "pop"
    -- | @pushl@ instruction
    pushl :: a -> I.Asm TextLabelCtx e ()
    pushl = intelSyntaxUnary "pushl"
    -- | @popl@ instruction
    popl :: a -> I.Asm TextLabelCtx e ()
    popl = intelSyntaxUnary "popl"
    -- | @idiv@ instruction
    idiv :: a -> I.Asm TextLabelCtx e ()
    idiv = intelSyntaxUnary "idiv"
    -- | @not@ instruction
    not :: a -> I.Asm TextLabelCtx e ()
    not = intelSyntaxUnary "not"

-- | @sete@ instruction
sete :: Register -> I.Asm TextLabelCtx e ()
sete = intelSyntaxUnary "sete"

-- | @setne@ instruction
setne :: Register -> I.Asm TextLabelCtx e ()
setne = intelSyntaxUnary "setne"

-- | @setl@ instruction
setl :: Register -> I.Asm TextLabelCtx e ()
setl = intelSyntaxUnary "setl"

-- | @setle@ instruction
setle :: Register -> I.Asm TextLabelCtx e ()
setle = intelSyntaxUnary "setle"

-- | @setg@ instruction
setg :: Register -> I.Asm TextLabelCtx e ()
setg = intelSyntaxUnary "setg"

-- | @setge@ instruction
setge :: Register -> I.Asm TextLabelCtx e ()
setge = intelSyntaxUnary "setge"

instance UnaryInstruction Integer
instance UnaryInstruction Int
instance UnaryInstruction Natural
instance UnaryInstruction Register
instance UnaryInstruction Offset
instance IsOperand a => UnaryInstruction (Ref a)

-- | A class of x86_64 instructions with binary arguments.
class Show a => BinaryInstruction a where
    -- | @mov@ instruction
    mov :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    mov = intelSyntaxBinary "mov"
    -- | @movl@ instruction
    movl :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    movl = intelSyntaxBinary "movl"
    -- | @movsx@ instruction
    movsx :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    movsx = intelSyntaxBinary "movsx"
    -- | @movsxd@ instruction
    movsxd :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    movsxd = intelSyntaxBinary "movsxd"
    -- | @movabs@ instruction
    movabs :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    movabs = intelSyntaxBinary "movabs"
    -- | @movzb@ instruction
    movzb :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    movzb = intelSyntaxBinary "movzb"
    -- | @cmp@ instruction
    cmp :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    cmp = intelSyntaxBinary "cmp"
    -- | @add@ instruction
    add :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    add = intelSyntaxBinary "add"
    -- | @sub@ instruction
    sub :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    sub = intelSyntaxBinary "sub"
    -- | @imul@ instruction
    imul :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    imul = intelSyntaxBinary "imul"
    -- | @and@ instruction
    and :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    and = intelSyntaxBinary "and"
    -- | @or@ instruction
    or :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    or = intelSyntaxBinary "or"
    -- | @xor@ instruction
    xor :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    xor = intelSyntaxBinary "xor"
    -- | @shl@ instruction
    shl :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    shl = intelSyntaxBinary "shl"
    -- | @sar@ instruction
    sar :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    sar = intelSyntaxBinary "sar"
    -- @lea@ instruction
    lea :: BinaryInstruction b => a -> b -> I.Asm TextLabelCtx e ()
    lea = intelSyntaxBinary "lea"

instance BinaryInstruction Integer
instance BinaryInstruction Int
instance BinaryInstruction Natural
instance BinaryInstruction Register
instance (IsOperand a, BinaryInstruction a) => BinaryInstruction (Ptr a)
instance IsOperand a => BinaryInstruction (Ref a)

-- | @cqo@ instruction
cqo :: I.Asm TextLabelCtx e ()
cqo = I.putStrLnWithIndent "cqo"

-- | @ret@ instruction
ret :: I.Asm TextLabelCtx e ()
ret = I.putStrLnWithIndent "ret"

-- | @leave@ instruction
leave :: I.Asm TextLabelCtx e ()
leave = I.putStrLnWithIndent "leave"

-- | @jmp@ instruction
jmp :: I.Asm TargetLabelCtx e () -> I.Asm TextLabelCtx e ()
jmp asm = I.putStrWithIndent "jmp " *> I.unCtx asm

-- | @je@ instruction
je :: I.Asm TargetLabelCtx e () -> I.Asm TextLabelCtx e ()
je asm = I.putStrWithIndent "je " *> I.unCtx asm

-- | @jne@ instruction
jne :: I.Asm TargetLabelCtx e () -> I.Asm TextLabelCtx e ()
jne asm = I.putStrWithIndent "jne " *> I.unCtx asm

-- | @jnz@ instruction
jnz :: I.Asm TargetLabelCtx e () -> I.Asm TextLabelCtx e ()
jnz asm = I.putStrWithIndent "jnz " *> I.unCtx asm

-- | @call@ instruction
call :: T.Text -> I.Asm TextLabelCtx e ()
call = intelSyntaxUnary "call"

