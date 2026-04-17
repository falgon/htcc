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
        f (ATNode (ATCallPtr (Just arg)) t l r) !s = f (ATNode (ATBlock arg) t l r) s
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
epilogue :: Ord i => CR.StorageClass i -> SI.Asm IT.TextLabelCtx e ()
epilogue ty = retLabel *> when (returnsBool ty) normalizeBoolRax *> IT.leave *> IT.ret
    where
        retLabel = SI.Asm $ \x -> do
            cf <- readIORef (SI.curFn x)
            unless (isJust cf) $ err "stray epilogue"
            T.hPutStrLn (SI.outHandle x) $ ".L.return." <> fromJust cf <> ":"

        returnsBool sc = case CR.toTypeKind sc of
            CR.CTFunc retTy _ -> retTy == CR.CTBool
            _                 -> False

normalizeBoolRax :: SI.Asm IT.TextLabelCtx e ()
normalizeBoolRax = normalizeBoolWordRax

normalizeBoolWordRax :: SI.Asm IT.TextLabelCtx e ()
normalizeBoolWordRax = IT.cmp rax (0 :: Int) *> IT.setne al *> IT.movzb rax al

normalizeBoolAbiRax :: SI.Asm IT.TextLabelCtx e ()
normalizeBoolAbiRax = IT.cmp al (0 :: Int) *> IT.setne al *> IT.movzb rax al

truncateRax :: Ord i => CR.StorageClass i -> SI.Asm IT.TextLabelCtx e ()
truncateRax t
    | CR.sizeof t == 1 = IT.movsx rax al
    | CR.sizeof t == 2 = IT.movsx rax ax
    | CR.sizeof t == 4 = IT.movsxd rax eax
    | otherwise = return ()

normalizeCallResultRax :: Ord i => CR.StorageClass i -> SI.Asm IT.TextLabelCtx e ()
normalizeCallResultRax t
    | CR.toTypeKind t == CR.CTBool = normalizeBoolAbiRax
    | needsAbiTruncation (CR.toTypeKind t) && CR.sizeof t < 8 = truncateRax t
    | otherwise = return ()
    where
        needsAbiTruncation ty = case ty of
            CR.CTChar     -> True
            CR.CTInt      -> True
            CR.CTEnum _ _ -> True
            CR.CTSigned x -> needsAbiTruncation x
            CR.CTShort x  -> needsAbiTruncation x
            CR.CTLong x   -> needsAbiTruncation x
            _             -> False

truncate :: Ord i => CR.StorageClass i -> SI.Asm IT.TextLabelCtx e ()
truncate ty = do
    IT.pop rax
    if CR.toTypeKind ty == CR.CTBool
        then normalizeBoolRax
        else truncateRax ty
    IT.push rax

genAddr :: (Integral e, Show e, IsOperand i, Integral i, Ord i, IT.UnaryInstruction i, IT.BinaryInstruction i) => ATree i -> SI.Asm IT.TextLabelCtx e ()
genAddr (ATNode (ATLVar _ v) _ _ _) = IT.lea rax (Ref $ rbp `osub` v) >> IT.push rax
genAddr (ATNode (ATGVar _ n) _ _ _) = IT.push (IT.Offset n)
genAddr (ATNode (ATFuncPtr n) _ _ _) = IT.push (IT.Offset n)
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

nonLoadableDerefType :: CR.StorageClass i -> Bool
nonLoadableDerefType ty =
    CR.isArray ty
        || case CR.toTypeKind ty of
            CR.CTFunc _ _ -> True
            _             -> False

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

genCallTarget :: (Show e, Integral e, Show i, Integral i, Ord i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i) => ATree i -> SI.Asm IT.TextLabelCtx e ()
genCallTarget callee
    | isFunctionDesignator callee = genAddr callee
    | otherwise = genStmt callee
    where
        isFunctionDesignator expr = case CR.toTypeKind (atype expr) of
            CR.CTFunc _ _ -> True
            _             -> False

stackArgCount :: [a] -> Int
stackArgCount = length . drop 6

callAligned
    :: (Show e, Integral e)
    => Int
    -> SI.Asm IT.TextLabelCtx e ()
    -> SI.Asm IT.TextLabelCtx e ()
    -> SI.Asm IT.TextLabelCtx e ()
callAligned nStackArgs prepare invoke = do
    let callPrepared = do
            prepare
            invoke
            cleanupStackArgs nStackArgs
    n <- IT.incrLbl
    IT.mov rax rsp
    when (odd nStackArgs) $
        IT.sub rax (8 :: Int)
    IT.and rax (0x0f :: Int)
    IT.jnz $ IT.ref "call" n
    callPrepared
    IT.jmp $ IT.refEnd n
    IT.label "call" n
    IT.sub rsp (8 :: Int)
    callPrepared
    IT.add rsp (8 :: Int)
    IT.end n

invokeIndirect :: (Show e, Integral e) => SI.Asm IT.TextLabelCtx e ()
invokeIndirect = do
    IT.mov rax (0 :: Int)
    IT.call "r11"

prepareCallArgs
    :: (Show e, Integral e, Show i, Integral i, Ord i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i)
    => [ATree i]
    -> SI.Asm IT.TextLabelCtx e ()
prepareCallArgs args = do
    let (nReg, _, stackArgs) = splitAtLen 6 args
        nArgs = nReg + length stackArgs
        slotRef base idx = Ref $ base `oadd` (8 * idx :: Int)
        storeValue base idx expr = do
            genStmt expr
            IT.pop rdx
            IT.mov (slotRef base idx) rdx
        restoreArgs base = do
            zipWithM_ (\reg idx -> IT.mov reg (slotRef base idx)) (reverse $ popRegs nReg) [0 .. pred nReg]
            mapM_ (IT.push . slotRef base) $ reverse [nReg .. pred nArgs]
    if nArgs == 0
        then pure ()
        else do
            IT.push rbx
            IT.sub rsp (8 * nArgs)
            IT.mov rbx rsp
            zipWithM_ (storeValue rbx) [0..] args
            IT.mov rax rbx
            IT.add rsp (8 * nArgs)
            IT.pop rbx
            restoreArgs rax

prepareIndirectCall
    :: (Show e, Integral e, Show i, Integral i, Ord i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i)
    => ATree i
    -> [ATree i]
    -> SI.Asm IT.TextLabelCtx e ()
prepareIndirectCall callee args = do
    let (nReg, _, stackArgs) = splitAtLen 6 args
        nArgs = nReg + length stackArgs
        calleeSlot = nArgs
        nSlots = succ nArgs
        slotRef base idx = Ref $ base `oadd` (8 * idx :: Int)
        storeValue base idx expr = do
            genStmt expr
            IT.pop rdx
            IT.mov (slotRef base idx) rdx
        restoreArgs base = do
            zipWithM_ (\reg idx -> IT.mov reg (slotRef base idx)) (reverse $ popRegs nReg) [0 .. pred nReg]
            IT.mov (rn 11) (slotRef base calleeSlot)
            mapM_ (IT.push . slotRef base) $ reverse [nReg .. pred nArgs]
    IT.push rbx
    IT.sub rsp (8 * nSlots)
    IT.mov rbx rsp
    genCallTarget callee
    IT.pop rdx
    IT.mov (slotRef rbx calleeSlot) rdx
    zipWithM_ (storeValue rbx) [0..] args
    IT.mov rax rbx
    IT.add rsp (8 * nSlots)
    IT.pop rbx
    restoreArgs rax

cleanupStackArgs :: Integral e => Int -> SI.Asm IT.TextLabelCtx e ()
cleanupStackArgs n =
    when (n > 0) $
        IT.add rsp (8 * n)

genStmt :: (Show e, Integral e, Show i, Integral i, Ord i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i) => ATree i -> SI.Asm IT.TextLabelCtx e ()
genStmt (ATNode (ATCallFunc x Nothing) t _ _) = do
    callAligned 0 (pure ()) $ IT.mov rax (0 :: Int) >> IT.call x
    normalizeCallResultRax t
    IT.push rax
genStmt (ATNode (ATCallPtr Nothing) t callee _) = do
    callAligned 0 (genCallTarget callee >> IT.pop (rn 11)) invokeIndirect
    normalizeCallResultRax t
    IT.push rax
genStmt (ATNode (ATCallFunc x (Just args)) t _ _) = do
    callAligned (stackArgCount args) (prepareCallArgs args) $ do
        IT.mov rax (0 :: Int)
        IT.call x
    normalizeCallResultRax t
    IT.push rax
genStmt (ATNode (ATCallPtr (Just args)) t callee _) = do
    callAligned (stackArgCount args) (prepareIndirectCall callee args) invokeIndirect
    normalizeCallResultRax t
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
    maybe (return ()) genStmt $ nonEmptyForClause isATForInit
    IT.begin n
    maybe
        (return ())
        ( \cond -> do
            genStmt cond
            IT.pop rax
            IT.cmp rax (0 :: Int)
            IT.je $ IT.refBreak n
        )
        $ nonEmptyForClause isATForCond
    maybe (return ()) genStmt $ nonEmptyForClause isATForStmt
    IT.continue n
    maybe (return ()) genStmt $ nonEmptyForClause isATForIncr
    IT.jmp $ IT.refBegin n
    IT.break n
    where
        nonEmptyForClause predicate =
            fromATKindFor
                <$> find
                    (\kind -> predicate kind && case fromATKindFor kind of
                        ATEmpty -> False
                        _       -> True
                    )
                    exps
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
genStmt (ATNode ATSizeof _ lhs _) = IT.push (fromIntegral (CR.sizeof $ atype lhs) :: Integer)
genStmt (ATNode ATAlignof _ lhs _) = IT.push (fromIntegral (CR.alignof $ atype lhs) :: Integer)
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
genStmt (ATNode ATComma _ lhs rhs) = genStmt lhs >> IT.add rsp (8 :: Int) >> genStmt rhs
genStmt (ATNode ATAddr _ lhs _) = genAddr lhs
genStmt (ATNode ATDeref t lhs _) = genStmt lhs >> unless (nonLoadableDerefType t) (load t)
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
genStmt n@(ATNode (ATFuncPtr _) _ _ _) = genAddr n
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

spillRegisterParam
    :: (Integral e, Ord i, IsOperand i, IT.BinaryInstruction i)
    => ATree i
    -> [Register]
    -> SI.Asm IT.TextLabelCtx e ()
spillRegisterParam (ATNode (ATLVar t o) _ _ _) regs
    | CR.toTypeKind t == CR.CTBool =
        maybe
            (SI.errCtx "internal compiler error: there is no full-width register for a _Bool parameter")
            (\fullReg -> IT.mov rax fullReg >> normalizeBoolAbiRax >> IT.mov (Ref $ rbp `osub` o) al)
            (find ((== 8) . byteWidth) regs)
    | otherwise =
        maybe
            (SI.errCtx "internal compiler error: there is no register that fits the specified size")
            (IT.mov (Ref $ rbp `osub` o))
            (find ((== CR.sizeof t) . byteWidth) regs)
spillRegisterParam _ _ =
    SI.errCtx "internal compiler error: expected local variable parameter slot"

spillStackParam
    :: (Integral e, Ord i, IsOperand i, IT.BinaryInstruction i)
    => Integer
    -> ATree i
    -> SI.Asm IT.TextLabelCtx e ()
spillStackParam callerOffset (ATNode (ATLVar t o) _ _ _) = case CR.sizeof t of
    1
        | CR.toTypeKind t == CR.CTBool ->
            loadCallerSlot >> normalizeBoolAbiRax >> IT.mov localSlot al
        | otherwise ->
            loadCallerSlot >> IT.mov localSlot al
    2 -> loadCallerSlot >> IT.mov localSlot ax
    4 -> loadCallerSlot >> IT.mov localSlot eax
    8 -> loadCallerSlot >> IT.mov localSlot rax
    _ -> SI.errCtx "internal compiler error: unsupported stack-passed parameter width"
    where
        localSlot = Ref $ rbp `osub` o
        callerSlot = Ref $ rbp `oadd` callerOffset
        loadCallerSlot = IT.mov rax callerSlot
spillStackParam _ _ =
    SI.errCtx "internal compiler error: expected local variable parameter slot"

textSection' :: (Integral e, Show e, Integral i, IsOperand i, IT.UnaryInstruction i, IT.BinaryInstruction i) => ATree i -> SI.Asm IT.TextSectionCtx e ()
textSection' lc@(ATNode (ATDefFunc fn margs) ty st _) = do
    unless (CR.isSCStatic ty) $ IT.global fn
    IT.fn fn $ do
        prologue (stackSize lc)
        when (isJust margs) $ do
            let (regArgs, stackArgs) = splitAt 6 $ fromJust margs
            zipWithM_ spillRegisterParam regArgs argRegs
            zipWithM_ spillStackParam [16, 24 ..] stackArgs
        genStmt st
        epilogue ty
textSection' ATEmpty = return ()
textSection' _ = SI.errCtx "internal compiler error: all abstract tree should start from some functions"

-- | data section of assembly code
dataSection :: (Show i, Ord i, Num i) => M.Map T.Text (GVar i) -> [Literal i] -> SI.Asm SI.AsmCodeCtx e ()
dataSection gvars lits = ID.dAta $ do
    forM_ lits $ \(Literal _ n cnt) -> ID.label (".L.data." <> tshow n) $ ID.byte cnt
    forM_ (M.toList gvars) $ \(var, GVar t ginit _) -> case ginit of
        PV.GVarInitWithZero    -> ID.label var $ ID.zero (CR.sizeof t)
        PV.GVarInitWithExternDecl -> pure ()
        PV.GVarInitWithOG ref  -> ID.label var $ ID.quad ref
        PV.GVarInitWithVal val -> ID.label var $ ID.sbyte (CR.sizeof t) val
        PV.GVarInitWithData ds -> ID.label var $ mapM_ emitInitData ds
        PV.GVarInitWithAST _   -> SI.errCtx "internal compiler error: unresolved global initializer AST"
    where
        emitInitData dat = case dat of
            PV.GVarInitZeroBytes sz -> ID.zero sz
            PV.GVarInitBytes sz val -> ID.sbyte sz val
            PV.GVarInitReloc sz ref addend
                | sz == 8 -> ID.quad $ formatReloc ref addend
                | otherwise -> SI.errCtx "internal compiler error: unsupported relocation width in global initializer"

        formatReloc ref addend
            | addend == 0 = ref
            | addend > 0 = ref <> "+" <> tshow addend
            | otherwise = ref <> tshow addend

-- | text section of assembly code
textSection :: (Integral e, Show e, IsOperand i, Integral i, Show i, IT.UnaryInstruction i, IT.BinaryInstruction i) => [ATree i] -> SI.Asm SI.AsmCodeCtx e ()
textSection atl = IT.text $ forM_ atl textSection'
