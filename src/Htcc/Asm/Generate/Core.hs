{-|
Module      : Htcc.Asm.Generate.Core
Description : The modules of intrinsic (x86_64) assembly
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

The modules of intrinsic (x86_64) assembly
-}
{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
module Htcc.Asm.Generate.Core (
    dataSection,
    textSection,
) where

import           Control.Monad                             (forM_, unless, when,
                                                            zipWithM_)
import           Control.Monad.Finally                     (MonadFinally (..))
import           Data.Int                                  (Int32)
import           Data.IORef                                (readIORef)
import           Data.List                                 (find)
import qualified Data.Map                                  as M
import           Data.Maybe                                (fromJust, isJust)
import qualified Data.Set                                  as S
import qualified Data.Text                                 as T
import qualified Data.Text.IO                              as T
import           Prelude                                   hiding (truncate)

import           Data.List                                 (foldl')
import           Data.Tuple.Extra                          (dupe, first, second)
import           Htcc.Asm.Intrinsic.Operand
import           Htcc.Asm.Intrinsic.Register
import qualified Htcc.Asm.Intrinsic.Structure              as SI
import qualified Htcc.Asm.Intrinsic.Structure.Section.Data as ID
import qualified Htcc.Asm.Intrinsic.Structure.Section.Text as IT
import qualified Htcc.CRules.Types                         as CR
import           Htcc.Parser                               (ATKind (..),
                                                            ATree (..),
                                                            fromATKindFor,
                                                            isATForCond,
                                                            isATForIncr,
                                                            isATForInit,
                                                            isATForStmt,
                                                            isComplexAssign)
import           Htcc.Parser.ConstructionData.Scope.Var    as PV
import           Htcc.Utils                                (err, maybe',
                                                            splitAtLen,
                                                            toNatural, tshow)
import           Numeric.Natural

stackSize :: (Show i, Integral i) => ATree i -> Natural
stackSize (ATNode (ATDefFunc _ args) _ body _) = let ms = f body $ maybe S.empty (foldr (\(ATNode (ATLVar t x) _ _ _) acc -> S.insert (t, x) acc) S.empty) args in
    if S.size ms == 1 then toNatural $ flip CR.alignas 8 $ toInteger $ CR.sizeof $ fst $ head (S.toList ms) else toNatural $ flip CR.alignas 8 $ uncurry (+) $
        first (toInteger . CR.sizeof . fst) $ second (fromIntegral . snd) $ dupe $ foldl' (\acc x -> if snd acc < snd x then x else acc) (CR.SCUndef CR.CTUndef, 0) $ S.toList ms
    where
        f ATEmpty !s = s
        f (ATNode (ATCallFunc _ (Just arg)) t l r) !s = f (ATNode (ATBlock arg) t l r) s
        f (ATNode (ATLVar t x) _ l r) !s = let i = S.insert (t, x) s in f l i `S.union` f r i
        f (ATNode (ATBlock xs) _ l r) !s = let i = foldr (S.union . (`f` s)) s xs in f l i `S.union` f r i
        f (ATNode (ATStmtExpr xs) t l r) !s = f (ATNode (ATBlock xs) t l r) s
        f (ATNode (ATFor xs) _ l r) !s = let i = foldr (S.union . flip f s . fromATKindFor) S.empty xs in f l i `S.union` f r i
        f (ATNode (ATNull x) _ _ _) !s = f x s
        f (ATNode _ _ l r) !s = f l s `S.union` f r s
stackSize _ = 0

{-# INLINE prologue #-}
prologue :: Integral i => i -> SI.Asm IT.TextLabelCtx e ()
prologue ss = IT.push rbp >> IT.mov rbp rsp >> IT.sub rsp (fromIntegral ss :: Integer)

{-# INLINE epilogue #-}
epilogue :: SI.Asm IT.TextLabelCtx e ()
epilogue = retLabel *> IT.leave *> IT.ret
    where
        retLabel = SI.Asm $ \x -> do
            cf <- readIORef (SI.curFn x)
            unless (isJust cf) $ err "stray epilogue"
            T.putStrLn $ ".L.return." <> fromJust cf <> ":"

truncate :: Ord i => CR.StorageClass i -> SI.Asm IT.TextLabelCtx e ()
truncate ty = do
    IT.pop rax
    when (CR.toTypeKind ty == CR.CTBool) $ IT.cmp rax (0 :: Int) *> IT.setne al
    truncate' ty
    IT.push rax
    where
        truncate' t
            | CR.sizeof t == 1 = IT.movsx rax al
            | CR.sizeof t == 2 = IT.movsx rax ax
            | CR.sizeof t == 4 = IT.movsxd rax eax
            | otherwise = return ()

genAddr :: (Integral e, Show e, IsOperand i, Integral i, Ord i, IT.UnaryInstruction i, IT.BinaryInstruction i) => ATree i -> SI.Asm IT.TextLabelCtx e ()
genAddr (ATNode (ATLVar _ v) _ _ _) = IT.lea rax (Ref $ rbp `osub` v) >> IT.push rax
genAddr (ATNode (ATGVar _ n) _ _ _) = IT.push (IT.Offset n)
genAddr (ATNode ATDeref _ lhs _) = genStmt lhs
genAddr (ATNode (ATMemberAcc m) _ lhs _) = do
    genAddr lhs
    IT.pop rax
    IT.add rax (CR.smOffset m)
    IT.push rax
genAddr _ = SI.errCtx "lvalue required as left operand of assignment"

genLVal :: (Integral e, Show e, IsOperand i, Integral i, Ord i, IT.UnaryInstruction i, IT.BinaryInstruction i) => ATree i -> SI.Asm IT.TextLabelCtx e ()
genLVal xs@(ATNode _ t _ _)
    | CR.isCTArray t = SI.errCtx "lvalue required as left operand of assignment"
    | otherwise = genAddr xs
genLVal _ = SI.errCtx "internal compiler error: genLVal catch ATEmpty"

load :: Ord i => CR.StorageClass i -> SI.Asm IT.TextLabelCtx e ()
load t
    | CR.sizeof t == 1 = IT.pop rax >> IT.movsx rax (IT.byte IT.Ptr (Ref rax)) >> IT.push rax
    | CR.sizeof t == 2 = IT.pop rax >> IT.movsx rax (IT.word IT.Ptr (Ref rax)) >> IT.push rax
    | CR.sizeof t == 4 = IT.pop rax >> IT.movsxd rax (IT.dword IT.Ptr (Ref rax)) >> IT.push rax
    | otherwise = IT.pop rax >> IT.mov rax (Ref rax) >> IT.push rax

store :: Ord i => CR.StorageClass i -> SI.Asm IT.TextLabelCtx e ()
store t = do
    IT.pop rdi
    IT.pop rax
    when (CR.toTypeKind t == CR.CTBool) $ IT.cmp rdi (0 :: Int) *> IT.setne dil *> IT.movzb rdi dil
    IT.mov (Ref rax) storeReg
    IT.push rdi
    where
        storeReg
            | CR.sizeof t == 1 = dil
            | CR.sizeof t == 2 = di
            | CR.sizeof t == 4 = edi
            | otherwise = rdi

increment :: Ord i => CR.StorageClass i -> SI.Asm IT.TextLabelCtx e ()
increment t = IT.pop rax >> IT.add rax (maybe 1 CR.sizeof $ CR.deref t) >> IT.push rax

decrement :: Ord i => CR.StorageClass i -> SI.Asm IT.TextLabelCtx e ()
decrement t = IT.pop rax >> IT.sub rax (maybe 1 CR.sizeof $ CR.deref t) >> IT.push rax

genStmt :: (Show e, Integral e, Show i, Integral i, Ord i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i) => ATree i -> SI.Asm IT.TextLabelCtx e ()
genStmt (ATNode (ATCallFunc x Nothing) _ _ _) = IT.call x >> IT.push rax
genStmt (ATNode (ATCallFunc x (Just args)) t _ _) = let (n', toReg, _) = splitAtLen 6 args in do
    mapM_ genStmt toReg
    mapM_ IT.pop $ popRegs n'
    n <- IT.incrLbl
    IT.mov rax rsp
    IT.and rax (0x0f :: Int)
    IT.jnz $ IT.ref "call" n
    IT.mov rax (0 :: Int)
    IT.call x
    IT.jmp $ IT.refEnd n
    IT.label "call" n
    IT.sub rsp (8 :: Int)
    IT.mov rax (0 :: Int)
    IT.call x
    IT.add rsp (8 :: Int)
    IT.end n
    when (CR.toTypeKind t == CR.CTBool) $ IT.movzb rax al
    IT.push rax
genStmt (ATNode (ATBlock stmt) _ _ _) = mapM_ genStmt stmt
genStmt (ATNode (ATStmtExpr stmt) _ _ _) = mapM_ genStmt stmt
genStmt (ATNode ATBreak _ _ _) = IT.jmp IT.refHBreak
genStmt (ATNode ATContinue _ _ _) = IT.jmp IT.refHContinue
genStmt (ATNode (ATGoto ident) _ _ _) = IT.jmp $ IT.refGoto ident
genStmt (ATNode (ATLabel ident) _ _ _) = IT.gotoLabel ident
genStmt (ATNode (ATFor exps) _ _ _) = IT.bracketBrkCnt $ do
    n <- IT.incrLbl
    IT.applyCnt >> IT.applyBrk
    maybe (return ()) (genStmt . fromATKindFor) $ find isATForInit exps
    IT.begin n
    maybe (return ()) (genStmt . fromATKindFor) $ find isATForCond exps
    IT.pop rax
    IT.cmp rax (0 :: Int)
    IT.je $ IT.refBreak n
    maybe (return ()) (genStmt . fromATKindFor) $ find isATForStmt exps
    IT.continue n
    maybe (return ()) (genStmt . fromATKindFor) $ find isATForIncr exps
    IT.jmp $ IT.refBegin n
    IT.break n
genStmt (ATNode ATWhile _ lhs rhs) = IT.bracketBrkCnt $ do
    n <- IT.incrLbl
    IT.applyCnt >> IT.applyBrk
    IT.continue n
    genStmt lhs
    IT.pop rax
    IT.cmp rax (0 :: Int)
    IT.je $ IT.refBreak n
    genStmt rhs
    IT.jmp $ IT.refContinue n
    IT.break n
genStmt (ATNode ATIf _ lhs rhs) = do
    genStmt lhs
    n <- IT.incrLbl
    IT.pop rax
    IT.cmp rax (0 :: Int)
    IT.je $ IT.refEnd n
    genStmt rhs
    IT.end n
genStmt (ATNode ATElse _ (ATNode ATIf _ llhs rrhs) rhs) = do
    genStmt llhs
    n <- IT.incrLbl
    IT.pop rax
    IT.cmp rax (0 :: Int)
    IT.je $ IT.refElse n
    genStmt rrhs
    IT.jmp $ IT.refEnd n
    IT.eLse n
    genStmt rhs
    IT.end n
genStmt (ATNode ATElse _ _ _) = SI.errCtx "internal compiler error: asm code generator should not reach here. Maybe abstract tree is broken it cause (bug)."
genStmt (ATNode (ATSwitch cond cases) _ _ _) = IT.bracketBrkCnt $ do
    n <- IT.incrLbl
    IT.applyBrk
    genStmt cond
    IT.pop rax
    ntr <- IT.makeCases cases
    IT.jmp $ IT.refBreak n
    mapM_ genStmt ntr
    IT.break n
genStmt (ATNode (ATCase n _) _ lhs _) = IT.cAse n >> genStmt lhs
genStmt (ATNode (ATDefault n) _ lhs _) = IT.cAse n >> genStmt lhs
genStmt (ATNode ATReturn t ATEmpty r) = genStmt $ ATNode ATReturn t (ATNode (ATNum 0) (CR.SCAuto CR.CTInt) ATEmpty ATEmpty) r
genStmt (ATNode ATReturn _ lhs _) = do
    genStmt lhs
    IT.pop rax
    IT.jmp IT.refReturn
genStmt (ATNode ATCast t lhs _) = genStmt lhs >> truncate t
genStmt (ATNode ATExprStmt _ lhs _) = genStmt lhs >> IT.add rsp (8 :: Int)
genStmt (ATNode ATBitNot _ lhs _) = do
    genStmt lhs
    IT.pop rax
    IT.not rax
    IT.push rax
genStmt (ATNode ATLAnd _ lhs rhs) = do
    n <- IT.incrLbl
    genStmt lhs >> IT.pop rax >> IT.cmp rax (0 :: Int) >> IT.je (IT.ref "false" n)
    genStmt rhs >> IT.pop rax >> IT.cmp rax (0 :: Int) >> IT.je (IT.ref "false" n)
    IT.push (1 :: Int)
    IT.jmp $ IT.refEnd n
    IT.label "false" n
    IT.push (0 :: Int)
    IT.end n
genStmt (ATNode ATLOr _ lhs rhs) = do
    n <- IT.incrLbl
    genStmt lhs >> IT.pop rax >> IT.cmp rax (0 :: Int) >> IT.jne (IT.ref "true" n)
    genStmt rhs >> IT.pop rax >> IT.cmp rax (0 :: Int) >> IT.jne (IT.ref "true" n)
    IT.push (0 :: Int)
    IT.jmp $ IT.refEnd n
    IT.label "true" n
    IT.push (1 :: Int)
    IT.end n
genStmt (ATNode (ATConditional cn ATEmpty el) _ _ _) = do
    n <- IT.incrLbl
    genStmt cn
    IT.pop rax
    IT.mov rdi rax
    IT.push rdi
    IT.cmp rax (0 :: Int)
    IT.je $ IT.refElse n
    IT.jmp $ IT.refEnd n
    IT.eLse n
    IT.pop rax
    genStmt el
    IT.end n
genStmt (ATNode (ATConditional cn th el) _ _ _) = do
    n <- IT.incrLbl
    genStmt cn
    IT.pop rax
    IT.cmp rax (0 :: Int)
    IT.je $ IT.refElse n
    genStmt th
    IT.jmp $ IT.refEnd n
    IT.eLse n
    genStmt el
    IT.end n
genStmt (ATNode ATPreInc t lhs _) = do
    genLVal lhs
    IT.push (Ref rsp)
    load t
    increment t
    store t
genStmt (ATNode ATPreDec t lhs _) = do
    genLVal lhs
    IT.push (Ref rsp)
    load t
    decrement t
    store t
genStmt (ATNode ATPostInc t lhs _) = do
    genLVal lhs
    IT.push (Ref rsp)
    load t
    increment t
    store t
    decrement t
genStmt (ATNode ATPostDec t lhs _) = do
    genLVal lhs
    IT.push (Ref rsp)
    load t
    decrement t
    store t
    increment t
genStmt (ATNode ATComma _ lhs rhs) = genStmt lhs >> genStmt rhs
genStmt (ATNode ATAddr _ lhs _) = genAddr lhs
genStmt (ATNode ATDeref t lhs _) = genStmt lhs >> unless (CR.isCTArray t) (load t)
genStmt (ATNode ATNot _ lhs _) = do
    genStmt lhs
    IT.pop rax
    IT.cmp rax (0 :: Int)
    IT.sete al
    IT.movzb rax al
    IT.push rax
genStmt (ATNode (ATNum x) _ _ _)
    | x <= fromIntegral (maxBound :: Int32) = IT.push x
    | otherwise = IT.movabs rax x >> IT.push rax
genStmt n@(ATNode (ATLVar _ _) t _ _) = genAddr n >> unless (CR.isCTArray t) (load t)
genStmt n@(ATNode (ATGVar _ _) t _ _) = genAddr n >> unless (CR.isCTArray t) (load t)
genStmt n@(ATNode (ATMemberAcc _) t _ _) = genAddr n >> unless (CR.isCTArray t) (load t)
genStmt (ATNode ATAssign t lhs rhs) = genLVal lhs >> genStmt rhs >> store t
genStmt (ATNode (ATNull _) _ _ _) = return ()
genStmt (ATNode kd ty lhs rhs)
    | isComplexAssign kd = do
        genLVal lhs
        IT.push (Ref rsp)
        load ty
        genStmt rhs
        binOp
        store ty
    | otherwise = genStmt lhs >> genStmt rhs >> binOp
    where
        binOp = flip finally (IT.push rax) $ IT.pop rdi *> IT.pop rax *> case kd of
            ATAdd -> IT.add rax rdi
            ATAddAssign -> IT.add rax rdi
            ATSub -> IT.sub rax rdi
            ATSubAssign -> IT.sub rax rdi
            ATAddPtr -> maybe' (SI.errCtx "the type is not pointer") (CR.deref ty) $ \dt -> do
                IT.imul rdi (fromIntegral (CR.sizeof dt) :: Int)
                IT.add rax rdi
            ATAddPtrAssign -> maybe' (SI.errCtx "the type is not pointer") (CR.deref ty) $ \dt -> do
                IT.imul rdi (fromIntegral (CR.sizeof dt) :: Int)
                IT.add rax rdi
            ATSubPtr -> maybe' (SI.errCtx "the type is not pointer") (CR.deref ty) $ \dt -> do
                IT.imul rdi (fromIntegral (CR.sizeof dt) :: Int)
                IT.sub rax rdi
            ATSubPtrAssign -> maybe' (SI.errCtx "the type is not pointer") (CR.deref ty) $ \dt -> do
                IT.imul rdi (fromIntegral (CR.sizeof dt) :: Int)
                IT.sub rax rdi
            ATPtrDis -> maybe' (SI.errCtx "the type is not pointer") (CR.deref $ atype lhs) $ \dt -> do
                IT.sub rax rdi
                IT.cqo
                IT.mov rdi (fromIntegral (CR.sizeof dt) :: Int)
                IT.idiv rdi
            ATMul -> IT.imul rax rdi
            ATMulAssign -> IT.imul rax rdi
            ATDiv -> IT.cqo >> IT.idiv rdi
            ATDivAssign -> IT.cqo >> IT.idiv rdi
            ATMod -> IT.cqo >> IT.idiv rdi >> IT.mov rax rdx
            ATAnd -> IT.and rax rdi
            ATAndAssign -> IT.and rax rdi
            ATOr -> IT.or rax rdi
            ATOrAssign -> IT.or rax rdi
            ATXor -> IT.xor rax rdi
            ATXorAssign -> IT.xor rax rdi
            ATShl -> IT.mov edx eax >> IT.mov rax rdi >> IT.mov ecx edx >> IT.shl rax cl
            ATShlAssign -> IT.mov edx eax >> IT.mov rax rdi >> IT.mov ecx edx >> IT.shl rax cl
            ATShr -> IT.push rax >> IT.mov rax rdi >> IT.mov edx eax >> IT.pop rax >> IT.mov ecx edx >> IT.sar rax cl
            ATShrAssign ->  IT.push rax >> IT.mov rax rdi >> IT.mov edx eax >> IT.pop rax >> IT.mov ecx edx >> IT.sar rax cl
            ATEQ -> IT.cmp rax rdi >> IT.sete al >> IT.movzb rax al
            ATNEQ -> IT.cmp rax rdi >> IT.setne al >> IT.movzb rax al
            ATLT -> IT.cmp rax rdi >> IT.setl al >> IT.movzb rax al
            ATLEQ -> IT.cmp rax rdi >> IT.setle al >> IT.movzb rax al
            ATGT -> IT.cmp rax rdi >> IT.setg al >> IT.movzb rax al
            ATGEQ -> IT.cmp rax rdi >> IT.setge al >> IT.movzb rax al
            _ -> SI.errCtx "internal compiler error: asm code generator should not reach here (binOp). Maybe abstract tree is broken it cause (bug)."
genStmt _ = return ()

textSection' :: (Integral e, Show e, Integral i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i) => ATree i -> SI.Asm IT.TextSectionCtx e ()
textSection' lc@(ATNode (ATDefFunc fn margs) ty st _) = do
    unless (CR.isSCStatic ty) $ IT.global fn
    IT.fn fn $ do
        prologue (stackSize lc)
        when (isJust margs) $ flip (`zipWithM_` fromJust margs) argRegs $ \(ATNode (ATLVar t o) _ _ _) reg ->
            maybe (SI.errCtx "internal compiler error: there is no register that fits the specified size")
                (IT.mov (Ref $ rbp `osub` o)) $ find ((== CR.sizeof t) . byteWidth) reg
        genStmt st
        epilogue
textSection' ATEmpty = return ()
textSection' _ = SI.errCtx "internal compiler error: all abstract tree should start from some functions"

-- | data section of assembly code
dataSection :: (Show i, Ord i, Num i) => M.Map T.Text (GVar i) -> [Literal i] -> SI.Asm SI.AsmCodeCtx e ()
dataSection gvars lits = ID.dAta $ do
    forM_ lits $ \(Literal _ n cnt) -> ID.label (".L.data." <> tshow n) $ ID.byte cnt
    forM_ (M.toList gvars) $ \(var, GVar t ginit) -> case ginit of
        PV.GVarInitWithZero    -> ID.label var $ ID.zero (CR.sizeof t)
        PV.GVarInitWithOG ref  -> ID.label var $ ID.quad ref
        PV.GVarInitWithVal val -> ID.label var $ ID.sbyte (CR.sizeof t) val

-- | text section of assembly code
textSection :: (Integral e, Show e, IsOperand i, Integral i, Show i, IT.UnaryInstruction i, IT.BinaryInstruction i) => [ATree i] -> SI.Asm SI.AsmCodeCtx e ()
textSection atl = IT.text $ forM_ atl textSection'
