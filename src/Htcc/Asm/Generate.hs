{-|
Module      : Htcc.Asm.Generate
Description : Assembly code generator
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Assembly code generator
-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Htcc.Asm.Generate (
    -- * Type
    InputCCode,
    -- * Generator
    casm
) where

-- Imports universal modules
import Prelude hiding (truncate)
import Data.Foldable (toList)
import Control.Exception (finally)
import Control.Monad (zipWithM_, when, unless)
import qualified Data.ByteString as B
import Data.List (find)
import Data.IORef (IORef, newIORef, readIORef, writeIORef) 
import Data.Either (either)
import Data.Int (Int32)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Sequence as S
import System.Exit (exitFailure)

-- Imports Tokenizer and parser
import Htcc.Utils (err, putStrLnErr, putStrErr, counter, tshow, toInts, splitAtLen, maybe')
import qualified Htcc.Tokenizer as HT
import Htcc.Parser (ATKind (..), ATree (..), fromATKindFor, isATForInit, isATForCond, isATForStmt, isATForIncr, isComplexAssign, parse, stackSize)
import Htcc.Parser.AST.Scope.Var (GVar (..), Literal (..))
import Htcc.Parser.AST.Scope.ManagedScope (ASTError)

-- Imports about assembly
import Htcc.Asm.Intrinsic.Register
import Htcc.Asm.Intrinsic.Operand
import qualified Htcc.Asm.Intrinsic.Instruction as I
import qualified Htcc.Asm.Intrinsic.Utils as I
import qualified Htcc.CRules.Types as CR

-- `Htcc.Asm.Generate.GenStatus` is status and information in code generation.
data GenStatus = GenStatus -- The constructor of `Htcc.Asm.Generate.GenStatus`
    {
        labelNumber :: IO Int, -- The label number. `labelNumber` is incremented each time it is read
        curFunc :: IORef (Maybe T.Text) -- `curFunc` stores the name of the function being processed
    }

{-# INLINE prologue #-}
prologue :: forall i. Integral i => i -> IO ()
prologue = T.putStr . T.append (I.push rbp <> I.mov rbp rsp) . I.sub rsp . (fromIntegral :: i -> Integer)

{-# INLINE epilogue #-}
epilogue :: Maybe T.Text -> IO () 
epilogue Nothing = err "internal compiler error: The function name cannot be tracked."
epilogue (Just fn) = T.putStr $ I.defLLbl (".return." <> fn <> ".") (0 :: Int) <> I.leave <> I.ret

{-# INLINE load #-}
load :: Ord i => CR.StorageClass i -> T.Text
load t 
    | CR.sizeof t == 1 = I.pop rax <> I.movsx rax (I.byte I.Ptr (Ref rax)) <> I.push rax
    | CR.sizeof t == 2 = I.pop rax <> I.movsx rax (I.word I.Ptr (Ref rax)) <> I.push rax
    | CR.sizeof t == 4 = I.pop rax <> I.movsxd rax (I.dword I.Ptr (Ref rax)) <> I.push rax
    | otherwise = I.pop rax <> I.mov rax (Ref rax) <> I.push rax

{-# INLINE store #-}
store :: Ord i => CR.StorageClass i -> T.Text
store ty = I.pop rdi <> I.pop rax <> booleanRound (CR.toTypeKind ty) <> store' ty <> I.push rdi
    where
        booleanRound CR.CTBool = I.cmp rdi (0 :: Int) <> I.setne dil <> I.movzb rdi dil
        booleanRound _ = ""
        store' t
            | CR.sizeof t == 1 = I.mov (Ref rax) dil
            | CR.sizeof t == 2 = I.mov (Ref rax) di
            | CR.sizeof t == 4 = I.mov (Ref rax) edi
            | otherwise = I.mov (Ref rax) rdi

{-# INLINE truncate #-}
truncate :: Ord i => CR.StorageClass i -> T.Text
truncate ty = I.pop rax <> booleanRound (CR.toTypeKind ty) <> truncate' ty <> I.push rax
    where   
        booleanRound CR.CTBool = I.cmp rax (0 :: Int) <> I.setne al
        booleanRound _ = ""
        truncate' t
            | CR.sizeof t == 1 = I.movsx rax al
            | CR.sizeof t == 2 = I.movsx rax ax
            | CR.sizeof t == 4 = I.movsxd rax eax
            | otherwise = ""

{-# INLINE increment #-}
increment :: Ord i => CR.StorageClass i -> T.Text
increment t = I.pop rax <> I.add rax (maybe 1 CR.sizeof $ CR.deref t) <> I.push rax

{-# INLINE decrement #-}
decrement :: Ord i => CR.StorageClass i -> T.Text
decrement t = I.pop rax <> I.sub rax (maybe 1 CR.sizeof $ CR.deref t) <> I.push rax

genAddr :: (Integral i, IsOperand i, I.UnaryInstruction i, I.BinaryInstruction i) => GenStatus -> ATree i -> IO ()
genAddr _ (ATNode (ATLVar _ v) _ _ _) = T.putStr $ I.lea rax (Ref $ rbp `osub` v) <> I.push rax
genAddr _ (ATNode (ATGVar _ n) _ _ _) = T.putStr $ I.push (I.Offset n)
genAddr c (ATNode ATDeref _ lhs _) = genStmt c lhs
genAddr c (ATNode (ATMemberAcc m) _ lhs _) = genAddr c lhs >> T.putStr (I.pop rax <> I.add rax (CR.smOffset m) <> I.push rax)
genAddr _ _ = err "lvalue required as left operand of assignment"

genLval :: (Integral i, IsOperand i, I.UnaryInstruction i, I.BinaryInstruction i) => GenStatus -> ATree i -> IO ()
genLval c xs@(ATNode _ t _ _)
    | CR.isCTArray t = err "lvalue required as left operand of assignment"
    | otherwise = genAddr c xs
genLval _ _ = err "internal compiler error: genLval catch ATEmpty"

-- | Simulate the stack machine by traversing an abstract syntax tree and output assembly codes.
genStmt :: (Integral i, IsOperand i, I.UnaryInstruction i, I.BinaryInstruction i) => GenStatus -> ATree i -> IO ()
genStmt c lc@(ATNode (ATDefFunc x Nothing) ty st _) = writeIORef (curFunc c) (Just x) >> T.putStr (if CR.isSCStatic ty then I.defLbl x else I.defGLbl x) >> prologue (stackSize lc) >> genStmt c st >> readIORef (curFunc c) >>= epilogue
genStmt c lc@(ATNode (ATDefFunc x (Just args)) ty st _) = do -- TODO: supports more than 7 arguments
    writeIORef (curFunc c) $ Just x
    T.putStr $ if CR.isSCStatic ty then I.defLbl x else I.defGLbl x
    prologue (stackSize lc)
    flip (`zipWithM_` args) argRegs $ \(ATNode (ATLVar t o) _ _ _) reg -> 
        maybe (err "internal compiler error: there is no register that fits the specified size") (T.putStr . I.mov (Ref $ rbp `osub` o)) $
            find ((== CR.sizeof t) . byteWidth) reg
    genStmt c st
    readIORef (curFunc c) >>= epilogue
genStmt _ (ATNode (ATCallFunc x Nothing) _ _ _) = T.putStr $ I.call x <> I.push rax
genStmt c (ATNode (ATCallFunc x (Just args)) t _ _) = let (n', toReg, _) = splitAtLen 6 args in do -- TODO: supports more than 7 arguments
    mapM_ (genStmt c) toReg
    mapM_ (T.putStr . I.pop) $ popRegs n'
    -- unless (null toStack) $ forM_ (reverse toStack) $ genStmt c
    n <- labelNumber c
    T.putStr $ I.mov rax rsp <> I.and rax (0x0f :: Int)
    T.putStr $ I.jnz $ I.refLLbl ".call." n
    T.putStr $ I.mov rax (0 :: Int) <> I.call x
    T.putStr $ I.jmp $ I.refLLbl ".end." n
    T.putStr $ I.defLLbl ".call." n
    T.putStr $ I.sub rsp (8 :: Int) <> I.mov rax (0 :: Int) <> I.call x <> I.add rsp (8 :: Int)
    T.putStr $ I.defLLbl ".end." n
    when (CR.toTypeKind t == CR.CTBool) $ T.putStr $ I.movzb rax al
    T.putStr $ I.push rax
genStmt c (ATNode (ATBlock stmts) _ _ _) = mapM_ (genStmt c) stmts
genStmt c (ATNode (ATStmtExpr stmts) _ _ _) = mapM_ (genStmt c) stmts
genStmt c (ATNode (ATFor exps) _ _ _) = do
    n <- labelNumber c
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForInit exps
    T.putStr $ I.defBegin n
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForCond exps
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refEnd n)
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForStmt exps
    maybe (return ()) (genStmt c . fromATKindFor) $ find isATForIncr exps
    T.putStr $ I.jmp $ I.refBegin n
    T.putStr $ I.defEnd n 
genStmt c (ATNode ATWhile _ lhs rhs) = do
    n <- labelNumber c
    T.putStr $ I.defBegin n
    genStmt c lhs
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refEnd n)
    genStmt c rhs
    T.putStr $ I.jmp $ I.refBegin n
    T.putStr $ I.defEnd n
genStmt c (ATNode ATIf _ lhs rhs) = do
    genStmt c lhs
    n <- labelNumber c
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refEnd n)
    genStmt c rhs
    T.putStr $ I.defEnd n
genStmt c (ATNode ATElse _ (ATNode ATIf _ llhs rrhs) rhs) = do
    genStmt c llhs
    n <- labelNumber c
    T.putStr $ I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refLLbl ".else." n)
    genStmt c rrhs
    T.putStr $ I.jmp (I.refEnd n)
    T.putStr $ I.defLLbl ".else." n
    genStmt c rhs
    T.putStr $ I.defEnd n
genStmt _ (ATNode ATElse _ _ _) = error "Asm code generator shold not reached here. Maybe abstract tree is broken it cause (bug)."
genStmt c (ATNode ATReturn t ATEmpty r) = genStmt c (ATNode ATReturn t (ATNode (ATNum 0) (CR.SCAuto CR.CTInt) ATEmpty ATEmpty) r) -- for return;
genStmt c (ATNode ATReturn _ lhs _) = genStmt c lhs >> T.putStr (I.pop rax) >> 
    readIORef (curFunc c) >>= maybe (err "The function name cannot be tracked.") (T.putStr . (\f -> I.jmp (I.refLLbl (".return." <> f <> ".") (0 :: Int))))
genStmt c (ATNode ATCast t lhs _) = genStmt c lhs >> T.putStr (truncate t)
genStmt c (ATNode ATExprStmt _ lhs _) = genStmt c lhs >> T.putStr (I.add rsp (8 :: Int))
genStmt c (ATNode ATBitNot _ lhs _) = genStmt c lhs >> T.putStr (I.pop rax <> I.not rax <> I.push rax)
genStmt c (ATNode ATLAnd _ lhs rhs) = do
    n <- labelNumber c
    genStmt c lhs >> T.putStr (I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refLLbl ".false." n))
    genStmt c rhs >> T.putStr (I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refLLbl ".false." n))
    T.putStr $ I.push (1 :: Int) <> I.jmp (I.refLLbl ".end." n) <> I.defLLbl ".false." n <> I.push (0 :: Int) <> I.defLLbl ".end." n
genStmt c (ATNode ATLOr _ lhs rhs) = do
    n <- labelNumber c
    genStmt c lhs >> T.putStr (I.pop rax <> I.cmp rax (0 :: Int) <> I.jne (I.refLLbl ".true." n))
    genStmt c rhs >> T.putStr (I.pop rax <> I.cmp rax (0 :: Int) <> I.jne (I.refLLbl ".true." n))
    T.putStr $ I.push (0 :: Int) <> I.jmp (I.refLLbl ".end." n) <> I.defLLbl ".true." n <> I.push (1 :: Int) <> I.defLLbl ".end." n
genStmt c (ATNode (ATConditional cn ATEmpty el) _ _ _) = do
    n <- labelNumber c
    genStmt c cn >> T.putStr (I.pop rax <> I.mov rdi rax <> I.push rdi <> I.cmp rax (0 :: Int) <> I.je (I.refLLbl ".else." n))
    T.putStr (I.jmp (I.refLLbl ".end." n) <> I.defLLbl ".else." n)
    T.putStr (I.pop rax) >> genStmt c el >> T.putStr (I.defLLbl ".end." n)
genStmt c (ATNode (ATConditional cn th el) _ _ _) = do
    n <- labelNumber c
    genStmt c cn >> T.putStr (I.pop rax <> I.cmp rax (0 :: Int) <> I.je (I.refLLbl ".else." n))
    genStmt c th >> T.putStr (I.jmp (I.refLLbl ".end." n) <> I.defLLbl ".else." n)
    genStmt c el >> T.putStr (I.defLLbl ".end." n)
genStmt c (ATNode ATPreInc t lhs _) = genLval c lhs >> T.putStr (I.push (Ref rsp) <> load t <> increment t <> store t) 
genStmt c (ATNode ATPreDec t lhs _) = genLval c lhs >> T.putStr (I.push (Ref rsp) <> load t <> decrement t <> store t) 
genStmt c (ATNode ATPostInc t lhs _) = genLval c lhs >> T.putStr (I.push (Ref rsp) <> load t <> increment t <> store t <> decrement t)
genStmt c (ATNode ATPostDec t lhs _) = genLval c lhs >> T.putStr (I.push (Ref rsp) <> load t <> decrement t <> store t <> increment t)
genStmt c (ATNode ATComma _ lhs rhs) = genStmt c lhs >> genStmt c rhs
genStmt c (ATNode ATAddr _ lhs _) = genAddr c lhs
genStmt c (ATNode ATDeref t lhs _) = genStmt c lhs >> unless (CR.isCTArray t) (T.putStr $ load t) 
genStmt c (ATNode ATNot _ lhs _) = genStmt c lhs >> T.putStr (I.pop rax <> I.cmp rax (0 :: Int) <> I.sete al <> I.movzb rax al <> I.push rax)
genStmt _ (ATNode (ATNum x) _ _ _) 
    | x <= fromIntegral (maxBound :: Int32) = T.putStr $ I.push x
    | otherwise = T.putStr $ I.movabs rax x <> I.push rax
genStmt c n@(ATNode (ATLVar _ _) t _ _) = genAddr c n >> unless (CR.isCTArray t) (T.putStr $ load t)
genStmt c n@(ATNode (ATGVar _ _) t _ _) = genAddr c n >> unless (CR.isCTArray t) (T.putStr $ load t)
genStmt c n@(ATNode (ATMemberAcc _) t _ _) = genAddr c n >> unless (CR.isCTArray t) (T.putStr $ load t)
genStmt c (ATNode ATAssign t lhs rhs) = genLval c lhs >> genStmt c rhs >> T.putStr (store t)
genStmt _ (ATNode (ATNull _) _ _ _) = return ()
genStmt c (ATNode kd ty lhs rhs)
    | isComplexAssign kd = genLval c lhs >> T.putStr (I.push (Ref rsp) <> load ty) >> genStmt c rhs >> f kd ty >> T.putStr (store ty)
    | otherwise = genStmt c lhs >> genStmt c rhs >> f kd ty
    where
        f k t = flip finally (T.putStr $ I.push rax) $ T.putStr (I.pop rdi) *> T.putStr (I.pop rax) *> case k of
            ATAdd -> T.putStr $ I.add rax rdi 
            ATAddAssign -> T.putStrLn $ I.add rax rdi
            ATSub -> T.putStr $ I.sub rax rdi 
            ATSubAssign -> T.putStr $ I.sub rax rdi
            ATAddPtr -> maybe' (err "The type is not pointer") (CR.deref t) $ \dt -> T.putStr $ I.imul rdi (fromIntegral (CR.sizeof dt) :: Int) <> I.add rax rdi
            ATAddPtrAssign -> maybe' (err "The type is not pointer") (CR.deref t) $ \dt -> T.putStr $ I.imul rdi (fromIntegral (CR.sizeof dt) :: Int) <> I.add rax rdi
            ATSubPtr -> maybe' (err "The type is not pointer") (CR.deref t) $ \dt -> T.putStr $ I.imul rdi (fromIntegral (CR.sizeof dt) :: Int) <> I.sub rax rdi
            ATSubPtrAssign -> maybe' (err "The type is not pointer") (CR.deref t) $ \dt -> T.putStr $ I.imul rdi (fromIntegral (CR.sizeof dt) :: Int) <> I.sub rax rdi
            ATPtrDis -> maybe' (err "The type is not pointer") (CR.deref $ atype lhs) $ \dt -> T.putStr $ I.sub rax rdi <> I.cqo <> I.mov rdi (fromIntegral (CR.sizeof dt) :: Int) <> I.idiv rdi
            ATMul -> T.putStr $ I.imul rax rdi 
            ATMulAssign -> T.putStr $ I.imul rax rdi
            ATDiv -> T.putStr $ I.cqo <> I.idiv rdi
            ATDivAssign -> T.putStr $ I.cqo <> I.idiv rdi
            ATMod -> T.putStr $ I.cqo <> I.idiv rdi <> I.mov rax rdx
            ATAnd -> T.putStr $ I.and rax rdi
            ATAndAssign -> T.putStr $ I.and rax rdi
            ATOr -> T.putStr $ I.or rax rdi 
            ATOrAssign -> T.putStr $ I.or rax rdi
            ATXor -> T.putStr $ I.xor rax rdi 
            ATXorAssign -> T.putStr $ I.xor rax rdi
            ATShl -> T.putStr $ I.mov edx eax <> I.mov rax rdi <> I.mov ecx edx <> I.shl rax cl 
            ATShlAssign -> T.putStr $ I.mov edx eax <> I.mov rax rdi <> I.mov ecx edx <> I.shl rax cl 
            ATShr -> T.putStr $ I.push rax <> I.mov rax rdi <> I.mov edx eax <> I.pop rax <> I.mov ecx edx <> I.sar rax cl  
            ATShrAssign -> T.putStr $ I.push rax <> I.mov rax rdi <> I.mov edx eax <> I.pop rax <> I.mov ecx edx <> I.sar rax cl  
            ATEQ -> T.putStr $ I.cmp rax rdi <> I.sete al <> I.movzb rax al
            ATNEQ -> T.putStr $ I.cmp rax rdi <> I.setne al <> I.movzb rax al
            ATLT -> T.putStr $ I.cmp rax rdi <> I.setl al <> I.movzb rax al
            ATLEQ -> T.putStr $ I.cmp rax rdi <> I.setle al <> I.movzb rax al
            ATGT -> T.putStr $ I.cmp rax rdi <> I.setg al <> I.movzb rax al
            ATGEQ -> T.putStr $ I.cmp rax rdi <> I.setge al <> I.movzb rax al
            _ -> err "Failed to assemble."
genStmt _ _ = return ()

{-# INLINE repSpace #-}
repSpace :: Integral i => i -> IO ()
repSpace = flip (>>) (putStrErr (T.singleton '^')) . mapM_ (putStrErr . T.pack . flip replicate ' ' . pred) . toInts

data MessageType = ErrorMessage | WarningMessage deriving (Eq, Ord, Enum)

instance Show MessageType where
    show ErrorMessage = "error"
    show WarningMessage = "warning"

-- | Input C code
type InputCCode = T.Text

{-# INLINE format #-}
format :: T.Text -> Int -> InputCCode -> IO ()
format errMesPre e xs = do
    putStrErr $ errMesPre <> " | "
    putStrLnErr (T.lines xs !! max 0 (fromIntegral e))
    putStrErr $ T.replicate (T.length errMesPre) " " <> " | "

tokenizeErrExit :: (Integral i, Show i) => InputCCode -> (HT.TokenLCNums i, T.Text) -> IO ()
tokenizeErrExit xs e = do
    putStrLnErr (tshow (fst e) <> ": error: stray '" <> snd e <> "' in program")
    format (T.replicate 4 " " <> tshow (HT.tkLn (fst e))) (pred $ fromIntegral $ HT.tkLn $ fst e) xs
    repSpace (HT.tkCn $ fst e) >> putStrLnErr ""
    exitFailure

parsedMessage :: (Integral i, Show i) => MessageType -> InputCCode -> ASTError i -> IO ()
parsedMessage mest xs (s, (i, etk)) = do
    putStrLnErr (tshow i <> ": " <> tshow mest <> ": " <> s)
    format (T.replicate 4 " " <> tshow (HT.tkLn i)) (pred $ fromIntegral $ HT.tkLn i) xs
    repSpace (HT.tkCn i) >> putStrLnErr (T.replicate (pred $ HT.length etk) "~")

parsedErrExit :: (Integral i, Show i) => InputCCode -> ASTError i -> IO ()
parsedErrExit = (.) (>> exitFailure) . parsedMessage ErrorMessage

parsedWarn :: (Integral i, Show i) => InputCCode -> S.Seq (ASTError i) -> IO ()
parsedWarn xs warns = mapM_ (parsedMessage WarningMessage xs) (toList warns)

dataSection :: Ord i => M.Map T.Text (GVar i) -> [Literal i] -> IO ()
dataSection gvars lits = do
    T.putStrLn ".data"
    mapM_ (\(Literal _ n cnt) -> T.putStrLn (".L.data." <> tshow n <> ":") >> T.putStr "\t.byte " >> T.putStrLn (T.intercalate ", " $ map tshow $ B.unpack cnt)) lits
    mapM_ (\(n, GVar t) -> T.putStrLn (n <> T.singleton ':') >> T.putStrLn ("\t.zero " <> tshow (CR.sizeof t))) $ M.toList gvars

textSection :: (Integral i, IsOperand i, I.UnaryInstruction i, I.BinaryInstruction i) => [ATree i] -> IO ()
textSection tk = do
    inc <- counter 0
    fname <- newIORef Nothing
    T.putStrLn ".text" >> mapM_ (genStmt $ GenStatus inc fname) tk

-- | Generate full assembly code from C language program
casm :: Bool -> InputCCode -> IO ()
casm supWarns xs = flip (either (tokenizeErrExit xs)) (f xs) $ \x -> 
    flip (either $ parsedErrExit xs) (parse x) $ \(warns, tk, gvars, lits) -> unless supWarns (parsedWarn xs warns) >> T.putStr I.declIS >> dataSection gvars lits >> textSection tk 
        where
            f = HT.tokenize :: T.Text -> Either (HT.TokenLCNums Integer, T.Text) [HT.TokenLC Integer]
