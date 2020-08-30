{-|
Module      : Htcc.Asm.Intrinsic.Register
Description : Types and classes of the x86_64 registers
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Types and classes of the x86_64 registers
-}
module Htcc.Asm.Intrinsic.Register (
    -- * Register class
    IsRegister (..),
    -- * Registers data types
    AccumulatorReg (..),
    BaseReg (..),
    CounterReg (..),
    DataReg (..),
    SrcIndexReg (..),
    DstIndexReg (..),
    BasePtrReg (..),
    StackPtrReg (..),
    ExtendedReg (..),
    Register (..),
    -- * Registers
    rax, eax, ax, ah, al,
    rbx, ebx, bx, bh, bl,
    rcx, ecx, cx, ch, cl,
    rdx, edx, dx, dh, dl,
    rsi, esi, si, sil,
    rdi, edi, di, dil,
    rbp, ebp, bp, bpl,
    rsp, esp, sp, spl,
    rn, rnd, rnw, rnb,
    -- * List of defined registers
    argRegs,
    popRegs
) where

import           Numeric.Natural

-- | The register type class
class Show a => IsRegister a where
    -- | The byte width of the register.
    byteWidth :: a -> Natural

-- | The accumulator register.
data AccumulatorReg = RAX -- ^ Full 64 bits of register
    | EAX -- ^ Lower 32 bits of register
    | AX -- ^ Lower 16 bits of register
    | AH -- ^ Lower 8 bits of register
    | AL -- ^ Upper 8 bits of `AX` register

instance Show AccumulatorReg where
    show RAX = "rax"
    show EAX = "eax"
    show AX  = "ax"
    show AH  = "ah"
    show AL  = "al"

instance IsRegister AccumulatorReg where
    byteWidth RAX = 8
    byteWidth EAX = 4
    byteWidth AX  = 2
    byteWidth AH  = 1
    byteWidth AL  = 1

instance Eq AccumulatorReg where
    (==) = flip (.) byteWidth . (==) . byteWidth

instance Ord AccumulatorReg where
    compare = flip (.) byteWidth . compare . byteWidth

-- | The Base register.
data BaseReg = RBX -- ^ Full 64 bits of register
    | EBX -- ^ Lower 32 bits of register
    | BX -- ^ Lower 16 bits of register
    | BH -- ^ Lower 8 bit register
    | BL -- ^ Upper 8 bit register of `BX` register

instance Show BaseReg where
    show RBX = "rbx"
    show EBX = "ebx"
    show BX  = "bx"
    show BH  = "bh"
    show BL  = "bl"

instance IsRegister BaseReg where
    byteWidth RBX = 8
    byteWidth EBX = 4
    byteWidth BX  = 2
    byteWidth BH  = 1
    byteWidth BL  = 1

instance Eq BaseReg where
    (==) = flip (.) byteWidth . (==) . byteWidth

instance Ord BaseReg where
    compare = flip (.) byteWidth . compare . byteWidth

-- | The Counter register.
data CounterReg = RCX -- ^ Full 64 bits of register
    | ECX -- ^ Lower 32 bits of register
    | CX -- ^ Lower 16 bits of register
    | CH -- ^ Lower 8 bit register
    | CL -- ^ Upper 8 bit register of `CX` register

instance Show CounterReg where
    show RCX = "rcx"
    show ECX = "ecx"
    show CX  = "cx"
    show CH  = "ch"
    show CL  = "cl"

instance IsRegister CounterReg where
    byteWidth RCX = 8
    byteWidth ECX = 4
    byteWidth CX  = 2
    byteWidth CH  = 1
    byteWidth CL  = 1

instance Eq CounterReg where
    (==) = flip (.) byteWidth . (==) . byteWidth

instance Ord CounterReg where
    compare = flip (.) byteWidth . compare . byteWidth

-- | The Data register.
data DataReg = RDX -- ^ Full 64 bits of register
    | EDX -- ^ Lower 32 bits of register
    | DX -- ^ Lower 16 bits of register
    | DH -- ^ Lower 8 bit register
    | DL -- ^ Upper 8 bit register of `DX` register

instance Show DataReg where
    show RDX = "rdx"
    show EDX = "edx"
    show DX  = "dx"
    show DH  = "dh"
    show DL  = "dl"

instance IsRegister DataReg where
    byteWidth RDX = 8
    byteWidth EDX = 4
    byteWidth DX  = 2
    byteWidth DH  = 1
    byteWidth DL  = 1

instance Eq DataReg where
    (==) = flip (.) byteWidth . (==) . byteWidth

instance Ord DataReg where
    compare = flip (.) byteWidth . compare . byteWidth

-- | The Source Index register.
data SrcIndexReg = RSI -- ^ Full 64 bits of register
    | ESI -- ^ Lower 32 bits of register
    | SI -- ^ Lower 16 bits of register
    | SIL -- ^ Lower 8 bits of register

instance Show SrcIndexReg where
    show RSI = "rsi"
    show ESI = "esi"
    show SI  = "si"
    show SIL = "sil"

instance IsRegister SrcIndexReg where
    byteWidth RSI = 8
    byteWidth ESI = 4
    byteWidth SI  = 2
    byteWidth SIL = 1

instance Eq SrcIndexReg where
    (==) = flip (.) byteWidth . (==) . byteWidth

instance Ord SrcIndexReg where
    compare = flip (.) byteWidth . compare . byteWidth

-- | The Destination Index register.
data DstIndexReg = RDI -- ^ Full 64 bits of register
    | EDI -- ^ Lower 32 bits of register
    | DI -- ^ Lower 16 bits of register
    | DIL -- ^ Lower 8 bits of register

instance Show DstIndexReg where
    show RDI = "rdi"
    show EDI = "edi"
    show DI  = "di"
    show DIL = "dil"

instance IsRegister DstIndexReg where
    byteWidth RDI = 8
    byteWidth EDI = 4
    byteWidth DI  = 2
    byteWidth DIL = 1

instance Eq DstIndexReg where
    (==) = flip (.) byteWidth . (==) . byteWidth

instance Ord DstIndexReg where
    compare = flip (.) byteWidth . compare . byteWidth

-- | The Base Pointer register.
data BasePtrReg = RBP -- ^ Full 64 bits of register
    | EBP -- ^ Lower 32 bits of register
    | BP -- ^ Lower 16 bits of register
    | BPL -- ^ Lower 8 bits of register

instance Show BasePtrReg where
    show RBP = "rbp"
    show EBP = "ebp"
    show BP  = "bp"
    show BPL = "bpl"

instance IsRegister BasePtrReg where
    byteWidth RBP = 8
    byteWidth EBP = 4
    byteWidth BP  = 2
    byteWidth BPL = 1

instance Eq BasePtrReg where
    (==) = flip (.) byteWidth . (==) . byteWidth

instance Ord BasePtrReg where
    compare = flip (.) byteWidth . compare . byteWidth

-- | The Stack Pointer register.
data StackPtrReg = RSP -- ^ Full 64 bits of register
    | ESP -- ^ Lower 32 bits of register
    | SP -- ^ Lower 16 bits of register
    | SPL -- ^ Lower 8 bits of register

instance Show StackPtrReg where
    show RSP = "rsp"
    show ESP = "esp"
    show SP  = "sp"
    show SPL = "spl"

instance IsRegister StackPtrReg where
    byteWidth RSP = 8
    byteWidth ESP = 4
    byteWidth SP  = 2
    byteWidth SPL = 1

instance Eq StackPtrReg where
    (==) = flip (.) byteWidth . (==) . byteWidth

instance Ord StackPtrReg where
    compare = flip (.) byteWidth . compare . byteWidth

-- | The extended general-purpose register (r8-r15).
data ExtendedReg = Rn Int -- ^ Full 64 bits of register
    | RnD Int -- ^ Lower 32 bits of register
    | RnW Int -- ^ Lower 16 bits of register
    | RnB Int -- ^ Lower 8 bits of register

instance Show ExtendedReg where
    show (Rn x)
        | x >= 8 && x <= 15 = "r" ++ show x
        | otherwise = error $ "Extended registers should be " ++ concat ["r" ++ show n ++ ", " | n <- [8..14] :: [Int]] ++ "or r15."
    show (RnD x)
        | x >= 8 && x <= 15 = "r" ++ show x ++ "d"
        | otherwise = error $ "Extended registers should be " ++ concat ["r" ++ show n ++ "d, " | n <- [8..14] :: [Int]] ++ "or r15d."
    show (RnW x)
        | x >= 8 && x <= 15 = "r" ++ show x ++ "w"
        | otherwise = error $ "Extended registers should be " ++ concat ["r" ++ show n ++ "w, " | n <- [8..14] :: [Int]] ++ "or r15w."
    show (RnB x)
        | x >= 8 && x <= 15 = "r" ++ show x ++ "b"
        | otherwise = error $ "Extended registers should be " ++ concat ["r" ++ show n ++ "b, " | n <- [8..14] :: [Int]] ++ "or r15b."

instance IsRegister ExtendedReg where
    byteWidth (Rn _)  = 8
    byteWidth (RnD _) = 4
    byteWidth (RnW _) = 2
    byteWidth (RnB _) = 1

instance Bounded ExtendedReg where
    minBound = Rn 8
    maxBound = Rn 15

instance Eq ExtendedReg where
    (==) = flip (.) byteWidth . (==) . byteWidth

instance Ord ExtendedReg where
    compare = flip (.) byteWidth . compare . byteWidth

-- | The registers.
data Register = Accumulator AccumulatorReg -- ^ The accumulator
    | Base BaseReg -- ^ The base
    | Counter CounterReg -- ^ The conuter
    | Data DataReg -- ^ The data
    | SrcIndex SrcIndexReg -- ^ The source index
    | DstIndex DstIndexReg -- ^ The destination index
    | BasePtr BasePtrReg -- ^ The base pointer index
    | StackPtr StackPtrReg -- ^ The stack pointer index
    | Extended  ExtendedReg -- ^ The extended general-purpose

instance Show Register where
    show (Accumulator x) = show x
    show (Base x)        = show x
    show (Counter x)     = show x
    show (Data x)        = show x
    show (SrcIndex x)    = show x
    show (DstIndex x)    = show x
    show (BasePtr x)     = show x
    show (StackPtr x)    = show x
    show (Extended x)    = show x

instance IsRegister Register where
    byteWidth (Accumulator x) = byteWidth x
    byteWidth (Base x)        = byteWidth x
    byteWidth (Counter x)     = byteWidth x
    byteWidth (Data x)        = byteWidth x
    byteWidth (SrcIndex x)    = byteWidth x
    byteWidth (DstIndex x)    = byteWidth x
    byteWidth (BasePtr x)     = byteWidth x
    byteWidth (StackPtr x)    = byteWidth x
    byteWidth (Extended x)    = byteWidth x

instance Eq Register where
    (==) = flip (.) byteWidth . (==) . byteWidth

instance Ord Register where
    compare = flip (.) byteWidth . compare . byteWidth

{-# INLINE rax #-}
-- | The rax register.
rax :: Register
rax = Accumulator RAX

{-# INLINE eax #-}
-- | The eax register. Lower 32 bits of `rax`.
eax :: Register
eax = Accumulator EAX

{-# INLINE ax #-}
-- | The ax register. Lower 16 bits of `rax`.
ax :: Register
ax = Accumulator AX

{-# INLINE ah #-}
-- | The ah register. Lower 8 bits of `rax`.
ah :: Register
ah = Accumulator AH

{-# INLINE al #-}
-- | The al register. Upper 8 bits of `ax`.
al :: Register
al = Accumulator AL

{-# INLINE rbx #-}
-- | The rbx register.
rbx :: Register
rbx = Base RBX

{-# INLINE ebx #-}
-- | The ebx register. Lower 32 bits of `rbx`.
ebx :: Register
ebx = Base RBX

{-# INLINE bx #-}
-- | The bx register. Lower 16 bits of `rbx`.
bx :: Register
bx = Base BX

{-# INLINE bh #-}
-- | The bh register. Lower 8 bits of `rbx`.
bh :: Register
bh = Base BH

{-# INLINE bl #-}
-- | The bl register. Upper 8 bits of `bx`.
bl :: Register
bl = Base BL

{-# INLINE rcx #-}
-- | The rcx register.
rcx :: Register
rcx = Counter RCX

{-# INLINE ecx #-}
-- | The ecx register. Lower 32 bis of `rcx`.
ecx :: Register
ecx = Counter ECX

{-# INLINE cx #-}
-- | The cx register. Lower 16 bits of `rcx`.
cx :: Register
cx = Counter CX

{-# INLINE ch #-}
-- | The ch register. Lower 8 bits of `rcx`.
ch :: Register
ch = Counter CH

{-# INLINE cl #-}
-- | The cl register. Upper 8 bits of `cx`.
cl :: Register
cl = Counter CL

{-# INLINE rdx #-}
-- | The rdx register.
rdx :: Register
rdx = Data RDX

{-# INLINE edx #-}
-- | The edx register. Lower 32 bits of `rdx`.
edx :: Register
edx = Data EDX

{-# INLINE dx #-}
-- | The dx register. Lower 16 bits of `rdx`.
dx :: Register
dx = Data DX

{-# INLINE dh #-}
-- | The dh register. Lower 8 bits of `rbx`.
dh :: Register
dh = Data DH

{-# INLINE dl #-}
-- | The dl register. Upper 8 bits of `dx`.
dl :: Register
dl = Data DL

{-# INLINE rsi #-}
-- | The rsi register.
rsi :: Register
rsi = SrcIndex RSI

{-# INLINE esi #-}
-- | The esi register. Lower 32 bits of `rsi`.
esi :: Register
esi = SrcIndex ESI

{-# INLINE si #-}
-- | The si register. Lower 16 bits of `rsi`.
si :: Register
si = SrcIndex SI

{-# INLINE sil #-}
-- | The sil register. Lower 8 bits of `rsi`.
sil :: Register
sil = SrcIndex SIL

{-# INLINE rdi #-}
-- | The rdi register.
rdi :: Register
rdi = DstIndex RDI

{-# INLINE edi #-}
-- | The edi register. Lower 32 bits of `rdi`.
edi :: Register
edi = DstIndex EDI

{-# INLINE di #-}
-- | The di register. Lower 16 bits of `rdi`.
di :: Register
di = DstIndex DI

{-# INLINE dil #-}
-- | The dil register. Lower 8 bits of `rdi`.
dil :: Register
dil = DstIndex DIL

{-# INLINE rbp #-}
-- | The rbp register.
rbp :: Register
rbp = BasePtr RBP

{-# INLINE ebp #-}
-- | The ebp register. Lower 32 bits of `rbp`.
ebp :: Register
ebp = BasePtr EBP

{-# INLINE bp #-}
-- | The bp register. Lower 16 bits of `rbp`.
bp :: Register
bp = BasePtr BP

{-# INLINE bpl #-}
-- | The bpl register. Lower 8 bits of `rbp`.
bpl :: Register
bpl = BasePtr BPL

{-# INLINE rsp #-}
-- | The rsp register.
rsp :: Register
rsp = StackPtr RSP

{-# INLINE esp #-}
-- | The esp register. Lower 32 bits of `rsp`.
esp :: Register
esp = StackPtr ESP

{-# INLINE sp #-}
-- | The sp register. Lower 16 bits of `rsp`.
sp :: Register
sp = StackPtr SP

{-# INLINE spl #-}
-- | The spl register. Lower 8 bits of `rsp`.
spl :: Register
spl = StackPtr SPL

{-# INLINE rn #-}
-- | The rn register (r8-r15).
rn :: Int -> Register
rn = Extended . Rn

{-# INLINE rnd #-}
-- | The rnd register (r8d-r15d). Lower 32 bits of `rn`.
rnd :: Int -> Register
rnd = Extended . RnD

{-# INLINE rnw #-}
-- | The rnw register (r8w-r15w). Lower 16 bits of `rn`.
rnw :: Int -> Register
rnw = Extended . RnW

{-# INLINE rnb #-}
-- | The rnb register (r8b-r15b). Lower 8 bits of `rn`.
rnb :: Int -> Register
rnb = Extended . RnB

{-# INLINE argRegs #-}
-- | `argRegs` is a list of each @Register@ used in the function call.
-- In the list, the registers used for the first to sixth arguments are arranged in ascending order.
argRegs :: [[Register]]
argRegs = [[dil, di, edi, rdi], [sil, si, esi, rsi], [dl, dx, edx, rdx], [cl, cx, ecx, rcx], [rnb 8, rnw 8, rnd 8, rn 8], [rnb 9, rnw 9, rnd 9, rn 9]]

{-# INLINE popRegs #-}
-- | `popRegs` is a list of registers used to @pop@ arguments from the stack in function calls.
-- This is equivalent to the following, but is defined explicitly for reducing time completity.
--
-- >>> popRegs 3 == map maximum (reverse (take 3 argRegs))
popRegs :: Int -> [Register]
popRegs = flip drop [rn 9, rn 8, rcx, rdx, rsi, rdi] . (6-)
