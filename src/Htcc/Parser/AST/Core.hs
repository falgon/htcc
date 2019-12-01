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
    -- * Abstract tree types and its relational type class
    ATKindFor (..),
    ATKind (..),
    ATree (..),
    Treealizable (..),
    -- * Constructor
    atNumLit,
    -- * Utilities
    isATForInit,
    isATForCond,
    isATForStmt,
    isATForIncr,
    fromATKindFor,
    isComplexAssign,
    isEmptyExprStmt,
    isEmptyReturn,
    isNonEmptyReturn,
    mapATKind,
    modifyTypeATKind
) where

import qualified Data.Text as T
import Control.Monad ((>=>))
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
    | ATAddAssign -- ^ addition assignment: @a += b@
    | ATSubAssign -- ^ subtraction assignment: @a -= b@
    | ATMulAssign -- ^ multiplication assignment: @a *= b@
    | ATDivAssign -- ^ division assignment: @a /= b@
    | ATAddPtrAssign -- ^ addition assignment for pointer: @p += q@
    | ATSubPtrAssign -- ^ subtraction assignment for pointer: @p -= q@
    | ATLAnd -- ^ logical and: @x && y@
    | ATLOr -- ^ logical or: @x || y@
    | ATAnd -- ^ bitwise and: @x & y@
    | ATAndAssign -- ^ bitwise and assignment: @a &= b@
    | ATOr -- ^ bitwise or: @x | y@
    | ATOrAssign -- ^ bitwise or assignment: @a |= b@
    | ATXor -- ^ bitwise xor: @x ^ y@
    | ATXorAssign -- ^ bitwise xor assignment: @a ^= b@
    | ATBitNot -- ^ bitwise not: @~x@
    | ATShl -- ^ left shift: @x << y@
    | ATShlAssign -- ^ left shift assignment: @a <<= b@
    | ATShr -- ^ right shift: @x >> y@
    | ATShrAssign -- ^ right shift assignment: @a >>= b@
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
    | ATConditional (ATree a) (ATree a) (ATree a) -- ^ conditional operator: @a ? x : y;@. It has three AST (cond, then and else)
    | ATComma -- ^ comma operator: @x,b@
    | ATCast -- ^ the cast operation: @(type) x@
    | ATMemberAcc (CT.StructMember a) -- ^ accessing the member of the @struct@
    | ATReturn -- ^ the @return@ keyword
    | ATIf -- ^ the @if@ keyword
    | ATElse -- ^ the @else@ keyword
    | ATSwitch (ATree a) [ATree a] -- ^ the @switch@ keyword, it has the conditional expression and compound statement by @case@s or @default@s
    | ATCase a a -- ^ the @case@ keyword, it has the value of label number and a constant value
    | ATDefault a -- ^ the @default@ keyword
    | ATWhile -- ^ the @while@ keyword
    | ATFor [ATKindFor a] -- ^ the @for@ keyword
    | ATBreak -- ^ the @break@ keyword
    | ATContinue -- ^ the @continue@ keyword
    | ATGoto  T.Text -- ^ the @goto@ keyword, it has name of the target label
    | ATLabel T.Text -- ^ the label, it has name of label.
    | ATBlock [ATree a] -- ^ the compound statement
    | ATLVar (CT.StorageClass a) a -- ^ the local variable. It has a type information (as `CT.StorageClass`) and an offset value
    | ATGVar (CT.StorageClass a) T.Text -- ^ the global variable. It has a type information (as `CT.StorageClass`) and an name
    | ATDefFunc T.Text (Maybe [ATree a]) -- ^ the function definition
    | ATCallFunc T.Text (Maybe [ATree a]) -- ^ the function call. It has a offset value and arguments (`Maybe`)
    | ATExprStmt -- ^ the expression of a statement
    | ATStmtExpr [ATree a] -- ^ the statement of a expression (GNU extension)
    | ATNull (ATree a) -- ^ indicates nothing to do
    deriving Show

{-# INLINE fromATVar #-}
-- | Take its type when it is ATIVar or ATIVar.
fromATVar :: ATKind i -> Maybe (CT.StorageClass i)
fromATVar (ATLVar s _) = Just s
fromATVar (ATGVar s _) = Just s
fromATVar _ = Nothing

instance IncompleteBase ATKind where
    isIncompleteArray = maybe False isIncompleteArray . fromATVar
    isIncompleteStruct = maybe False isIncompleteStruct . fromATVar
    fromIncompleteArray = fromATVar >=> fromIncompleteArray
    fromIncompleteStruct = fromATVar >=> fromIncompleteStruct
    isValidIncomplete = maybe False isValidIncomplete . fromATVar 

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
isComplexAssign ATShlAssign = True
isComplexAssign ATShrAssign = True
isComplexAssign _ = False

-- | The data structure of abstract syntax tree
data ATree a = ATEmpty -- ^ The empty node 
    | ATNode { 
    atkind :: ATKind a, -- ^ The kind of abstract tree
    atype :: CT.StorageClass a, -- ^ The data type
    atL :: ATree a, -- ^ The left hand side abstract tree
    atR :: ATree a -- ^ The right hand side abstract tree
    } -- ^ `ATKind` representing the kind of node and the two branches `ATree` it has
    deriving Show

-- | A class whose type can be converted to ATree
class Treealizable a where
    -- | Convert to `ATree`
    treealize :: a i -> ATree i

{-# INLINE isEmptyExprStmt #-}
-- | `isEmptyExprStmt` returns `True` only if both sides of `ATExprStmt` are `ATEmpty`. Otherwise, returns `False`.
isEmptyExprStmt :: ATree a -> Bool
isEmptyExprStmt (ATNode ATExprStmt _ ATEmpty ATEmpty) = True
isEmptyExprStmt _ = False

{-# INLINE isNonEmptyReturn #-}
-- | `isNonEmptyReturn` returns `True` only if the `ATKind` of the given argument is `ATReturn` and the left side hand node of the given argument is not `ATEmpty`.
-- Otherwise, returns `False`.
isNonEmptyReturn :: ATree a -> Bool
isNonEmptyReturn (ATNode ATReturn _ ATEmpty _) = False
isNonEmptyReturn (ATNode ATReturn _ _ _) = True
isNonEmptyReturn _ = False

{-# INLINE isEmptyReturn #-}
-- | `isEmptyReturn` returns `True` only if the `ATKind` of the given argument is `ATReturn` and the left side hand node of the given argument is `ATEmpty`.
-- Otherwise, returns `False`.
isEmptyReturn :: ATree a -> Bool
isEmptyReturn (ATNode ATReturn _ ATEmpty _) = True
isEmptyReturn _ = False

{-# INLINE atNumLit #-}
-- | `atNumLit` is a shortcut for constructing a numeric literal node
atNumLit :: i -> ATree i
atNumLit = flip (flip (flip ATNode (CT.SCAuto $ CT.CTLong CT.CTInt)) ATEmpty) ATEmpty . ATNum

-- | mapping for `ATKind`
mapATKind :: (ATKind i -> ATKind i) -> ATree i -> ATree i
mapATKind f (ATNode atk t l r) = ATNode (f atk) t (mapATKind f l) (mapATKind f r)
mapATKind _ ATEmpty = ATEmpty

-- | applying for `Htcc.CRules.Types.StorageClass.StorageClass` of `ATLVar` or `ATGVar`
modifyTypeATKind :: (CT.StorageClass i -> CT.StorageClass i) -> ATKind i -> ATKind i
modifyTypeATKind f (ATLVar t o) = ATLVar (f t) o
modifyTypeATKind f (ATGVar t o) = ATGVar (f t) o
modifyTypeATKind _ _ = ATNull ATEmpty
