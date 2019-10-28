{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Htcc.Parser.AST.Core
Description : The AST data type and its utilities
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The AST data type and its utilities
-}
module Htcc.Parser.AST.Core (
    -- * Abstract tree types
    ATKindFor (..),
    ATKind (..),
    ATree (..),
    -- * Utilities
    isATForInit,
    isATForCond,
    isATForStmt,
    isATForIncr,
    fromATKindFor,
    isEmptyExprStmt
) where

import qualified Data.Text as T
import Htcc.CRules.Types as CT

-- | Specially @for@ syntax tree type
data ATKindFor a = ATForkw -- ^ The @for@ keyword
    | ATForInit (ATree a) -- ^ The initial section of @for@ statement
    | ATForCond (ATree a) -- ^ The conditional section of @for@ statement
    | ATForIncr (ATree a) -- ^ The incremental section of @for@ statement
    | ATForStmt (ATree a) -- ^ The statement section of @for@ statement
    deriving Show

{-# INLINE isATForInit #-}
-- | An utility of `ATForInit`. When an argument is `ATForInit`, return `True` otherwise `False`
isATForInit :: ATKindFor a -> Bool
isATForInit (ATForInit _) = True
isATForInit _ = False

{-# INLINE isATForCond #-}
-- | An utility of `ATForCond`. When an argument is `ATForCond`, return `True` otherwise `False`
isATForCond :: ATKindFor a -> Bool
isATForCond (ATForCond _) = True
isATForCond _ = False

{-# INLINE isATForStmt #-}
-- | An utility `ATForStmt`. When an argument is `ATForStmt`, return `True` otherwise `False`
isATForStmt :: ATKindFor a -> Bool
isATForStmt (ATForStmt _) = True
isATForStmt _ = False

{-# INLINE isATForIncr #-}
-- | An utility `ATForIncr`. When an argument is `ATForIncr`, return `True` otherwise `False`
isATForIncr :: ATKindFor a -> Bool
isATForIncr (ATForIncr _) = True
isATForIncr _ = False

{-# INLINE fromATKindFor #-}
-- | take ATree data from `ATKindFor`.
fromATKindFor :: ATKindFor a -> ATree a
fromATKindFor (ATForInit x) = x
fromATKindFor (ATForCond x) = x
fromATKindFor (ATForIncr x) = x
fromATKindFor (ATForStmt x) = x
fromATKindFor _ = error "ATKindFor is ATForkw"

-- | The syntax tree type. Let \(x,y\in\mathbb{N}\), Let \(p\) and \(q\) be pointers to variables \(a\) and \(b\), respectively (@p=&a,q=&b@).
data ATKind a = ATAdd -- ^ \(x+y\): @x + y@
    | ATAddPtr -- ^ Add operation for pointer \(p+x,x+p\): @p + x, x + p@
    | ATSub -- ^ \(x-y\): @x - y@
    | ATSubPtr -- ^ Sub operation for pointer \(p-x\): @p - x@
    | ATPtrDis -- ^ The distance of pointers \(p-q\): @p - q@
    | ATMul -- ^ \(x\times y\): @x * y@
    | ATDiv -- ^ \(x\div y\): @x / y@
    | ATMod -- ^ \(x\bmod y\): @x % y@
    | ATAnd -- ^ bitwise and: @x & y@
    | ATOr -- ^ bitwise or: @x | y@
    | ATXor -- ^ bitwise xor: @x ^ y@
    | ATBitNot -- ^ bitwise not: @~x@
    | ATShl -- ^ left shift: @x << y@
    | ATShr -- ^ right shift: @x >> y@
    | ATLT  -- ^ \(x\lt y\): @x < y@
    | ATLEQ -- ^ \(x\leq y\): @x <= y@
    | ATGT  -- ^ \(x\gt y\): @x > y@
    | ATGEQ -- ^ \(x\geq y\): @x >= y@
    | ATEQ  -- ^ \(x=y\): @x == y@
    | ATNEQ -- ^ \(x\not= y\): @x != y@
    | ATNot -- ^ The not operator @!@: @!x@
    | ATAddr -- ^ The addressing operator @&@: @&x@
    | ATDeref -- ^ The dereferencing operator @*@: @*p@
    | ATAssign -- ^ The assign operator: @x=y@
    | ATNum a -- ^ The number
    | ATCast -- ^ The cast operation: @(type) x@
    | ATMemberAcc (CT.StructMember a) -- ^ Accessing the member of the @struct@
    | ATReturn -- ^ The @return@ keyword
    | ATIf -- ^ The @if@ keyword
    | ATElse -- ^ The @else@ keyword
    | ATWhile -- ^ The @while@ keyword
    | ATFor [ATKindFor a] -- ^ The @for@ keyword
    | ATBlock [ATree a] -- ^ The compound statement
    | ATLVar (CT.TypeKind a) a -- ^ The local variable. It has a type information (as `CT.TypeKind`) and an offset value
    | ATGVar (CT.TypeKind a) T.Text -- ^ The global variable. It has a type information (as `CT.TypeKind`) and an name
    | ATDefFunc T.Text (Maybe [ATree a]) -- ^ The function definition
    | ATCallFunc T.Text (Maybe [ATree a]) -- ^ The function call. It has a offset value and arguments (`Maybe`)
    | ATExprStmt -- ^ The expression of a statement
    | ATStmtExpr [ATree a] -- ^ The statement of a expression (GNU extension)
    | ATNull (ATree a) -- ^ Indicates nothing to do
    deriving Show

-- | The data structure of abstract syntax tree
data ATree a = ATEmpty -- ^ The empty node 
    | ATNode { 
    atkind :: ATKind a, -- ^ The kind of abstract tree
    atype :: CT.TypeKind a, -- ^ The data type
    atL :: ATree a, -- ^ The left hand side abstract tree
    atR :: ATree a -- ^ The right hand side abstract tree
    } -- ^ `ATKind` representing the kind of node and the two branches `ATree` it has
    deriving Show

{-# INLINE isEmptyExprStmt #-}
-- | `isEmptyExprStmt` returns `True` only if both sides of `ATExprStmt` are `ATEmpty`. Otherwise, returns `False`.
isEmptyExprStmt :: ATree a -> Bool
isEmptyExprStmt (ATNode ATExprStmt _ ATEmpty ATEmpty) = True
isEmptyExprStmt _ = False
