{-|
Module      : Htcc.Asm.Intrinsic.Register
Description : Types and classes of the x86_64 operands
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Types and classes of the x86_64 operands
-}
module Htcc.Asm.Intrinsic.Operand (
    -- * The operand classes and types.
    IsOperand (..),
    Operand (..),
    Ref (..)
) where

import Htcc.Asm.Intrinsic.Register (Register (..))

-- | The operand type.
newtype Operand = Operand String -- ^ The constructor of `Operand`.

instance Show Operand where
    show (Operand x) = x

-- | `IsOperand` class has an operand type as instances.
class Show a => IsOperand a where
    -- | The operation of add.
    oadd :: IsOperand b => a -> b -> Operand
    oadd x y = Operand $ show x ++ "+" ++ show y
    -- | The operation of sub.
    osub :: IsOperand b => a -> b -> Operand
    osub x y = Operand $ show x ++ "-" ++ show y
    -- | The operation of mul.
    omul :: IsOperand b => a -> b -> Operand
    omul x y = Operand $ show x ++ "*" ++ show y
    -- | The operation of div.
    odiv :: IsOperand b => a -> b -> Operand
    odiv x y = Operand $ show x ++ "/" ++ show y

instance IsOperand Operand
instance IsOperand Int
instance IsOperand Integer
instance IsOperand Register

-- | The type that specifies that register values are considered address values.
-- e.g.:
--
-- >>> Ref rax
-- [rax]
-- >>> Ref rsp
-- [rsp]
-- >>> import qualified Data.Text as T
-- >>> T.putStr $ mov rax (Ref rsp) <> add rsp 8
--      mov rax, [rsp]
--      add rsp, 8
newtype Ref a = Ref a -- ^ The constructor of `Ref`.

instance IsOperand a => Show (Ref a) where
    show (Ref x) = "[" ++ show x ++ "]"
