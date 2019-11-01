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
    isComplexAssign,
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
    | ATAddPtr -- ^ add operation for pointer \(p+x,x+p\): @p + x, x + p@
    | ATSub -- ^ \(x-y\): @x - y@
    | ATSubPtr -- ^ sub operation for pointer \(p-x\): @p - x@
    | ATPtrDis -- ^ The distance of pointers \(p-q\): @p - q@
    | ATMul -- ^ \(x\times y\): @x * y@
    | ATDiv -- ^ \(x\div y\): @x / y@
    | ATMod -- ^ \(x\bmod y\): @x % y@
    | ATAddAssign -- ^ addition assignment: @x += y@
    | ATSubAssign -- ^ subtraction assignment: @x -= y@
    | ATMulAssign -- ^ multiplication assignment: @x *= y@
    | ATDivAssign -- ^ division assignment: @x /= y@
    | ATAddPtrAssign -- ^ addition assignment for pointer: @p += q@
    | ATSubPtrAssign -- ^ subtraction assignment for pointer: @p -= q@
    | ATLAnd -- ^ logical and: @x && y@
    | ATLOr -- ^ logical or: @x || y@
    | ATAnd -- ^ bitwise and: @x & y@
    | ATAndAssign -- ^ bitwise and assignment: @x &= y@
    | ATOr -- ^ bitwise or: @x | y@
    | ATOrAssign -- ^ bitwise or assignment: @x |= y@
    | ATXor -- ^ bitwise xor: @x ^ y@
    | ATXorAssign -- ^ bitwise xor assignment: @x ^= y@
    | ATBitNot -- ^ bitwise not: @~x@
    | ATShl -- ^ left shift: @x << y@
    | ATShr -- ^ right shift: @x >> y@
    | ATLT  -- ^ \(x\lt y\): @x < y@
    | ATLEQ -- ^ \(x\leq y\): @x <= y@
    | ATGT  -- ^ \(x\gt y\): @x > y@
    | ATGEQ -- ^ \(x\geq y\): @x >= y@
    | ATEQ  -- ^ \(x=y\): @x == y@
    | ATNEQ -- ^ \(x\not= y\): @x != y@
    | ATNot -- ^ not operator @!@: @!x@
    | ATAddr -- ^ addressing operator @&@: @&x@
    | ATDeref -- ^ dereferencing operator @*@: @*p@
    | ATAssign -- ^ assign operator: @x=y@
    | ATPreInc -- ^ pre-increment operator: @++a@
    | ATPreDec -- ^ pre-decrement operator: @--a@
    | ATPostInc -- ^ post-increment operator: @a++@
    | ATPostDec -- ^ post-decrement operator: @a--@
    | ATNum a -- ^ The number
    | ATComma -- ^ comma operator: @,@
    | ATCast -- ^ the cast operation: @(type) x@
    | ATMemberAcc (CT.StructMember a) -- ^ accessing the member of the @struct@
    | ATReturn -- ^ the @return@ keyword
    | ATIf -- ^ the @if@ keyword
    | ATElse -- ^ the @else@ keyword
    | ATWhile -- ^ the @while@ keyword
    | ATFor [ATKindFor a] -- ^ the @for@ keyword
    | ATBlock [ATree a] -- ^ the compound statement
    | ATLVar (CT.TypeKind a) a -- ^ the local variable. It has a type information (as `CT.TypeKind`) and an offset value
    | ATGVar (CT.TypeKind a) T.Text -- ^ the global variable. It has a type information (as `CT.TypeKind`) and an name
    | ATDefFunc T.Text (Maybe [ATree a]) -- ^ the function definition
    | ATCallFunc T.Text (Maybe [ATree a]) -- ^ the function call. It has a offset value and arguments (`Maybe`)
    | ATExprStmt -- ^ the expression of a statement
    | ATStmtExpr [ATree a] -- ^ the statement of a expression (GNU extension)
    | ATNull (ATree a) -- ^ indicates nothing to do
    deriving Show

{-# INLINE isComplexAssign #-}
-- | Returns True if the given `ATKind` is an assignment operator other than simple assignment. 
-- Otherwise, returns `False`.
isComplexAssign :: ATKind a -> Bool
isComplexAssign ATAddAssign = True
isComplexAssign ATSubAssign = True
isComplexAssign ATMulAssign = True
isComplexAssign ATDivAssign = True
isComplexAssign ATAddPtrAssign = True
isComplexAssign ATSubPtrAssign = True
isComplexAssign ATOrAssign = True
isComplexAssign ATAndAssign = True
isComplexAssign ATXorAssign = True
isComplexAssign _ = False

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
