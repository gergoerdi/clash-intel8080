{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Hardware.Intel8080.Microcode where

import Prelude ()
import Clash.Prelude

import Data.Singletons.TH hiding (NFDataX)
import Hardware.Intel8080
import Hardware.Intel8080.Amble

$(singletons [d|
  data Addressing
      = Indirect
      | Port
      | IncrPC
      | IncrSP
      | DecrSP
  |])
deriving instance Show Addressing

data UpdateA
    = SetA
    | KeepA
    deriving (Show, Eq, Generic, NFDataX)

data UpdateC
    = SetC
    | KeepC
    deriving (Show, Eq, Generic, NFDataX)

data Effect
    = Get Reg
    | Set Reg
    | ToBuf Target
    | FromBuf Target
    | Get2 RegPair
    | Swap2 RegPair
    | Jump
    | ReadMem
    | When (Maybe Cond)
    | Compute ALUArg ALU UpdateC UpdateA
    | Compute2 ALU2 UpdateC
    | Compute0 Flag ALU0
    | UpdateFlags
    | Rst (Unsigned 3)
    | SetInt Bool
    | FixupBCD
    deriving (Show, Generic, NFDataX)

data ALUArg
    = RegA
    | Const01
    | ConstFF
    deriving (Show, Generic, NFDataX)

data ALU2
    = Inc2
    | Dec2
    | AddHL
    deriving (Show, Generic, NFDataX)

data ALU0
    = Complement0
    | ConstTrue0
    deriving (Show, Generic, NFDataX)

data Target
    = AddrBuf
    | PC
    deriving (Show, Generic, NFDataX)

type MacroSteps n ends = Amble n ends Effect

imm1 = step @(Just IncrPC) @Nothing ReadMem >:> End

imm2 =
    step @(Just IncrPC) @Nothing ReadMem >:>
    step @Nothing       @Nothing (FromBuf AddrBuf) >:>
    step @(Just IncrPC) @Nothing ReadMem >:>
    step @Nothing       @Nothing (FromBuf AddrBuf) >:>
    End

push2 =
    step @Nothing @(Just DecrSP) (ToBuf AddrBuf) >:>
    step @Nothing @(Just DecrSP) (ToBuf AddrBuf) >:>
    End

pushPC =
    step @Nothing @(Just DecrSP) (ToBuf PC) >:>
    step @Nothing @(Just DecrSP) (ToBuf PC) >:>
    End

pop2 =
    step @(Just IncrSP) @Nothing ReadMem >:>
    step @Nothing       @Nothing (FromBuf AddrBuf) >:>
    step @(Just IncrSP) @Nothing ReadMem >:>
    step @Nothing       @Nothing (FromBuf AddrBuf) >:>
    End

shiftRotate op =
    step @Nothing @Nothing (Get rA) >:>
    step @Nothing @Nothing (Compute RegA op SetC KeepA) >:>
    step @Nothing @Nothing (Set rA) >:>
    End

type RW = Maybe (Either Addressing Addressing)
type MicroOp = (Effect, RW)
type MicroLen = 10
type Microcode = (Maybe Addressing, Vec MicroLen MicroOp)

mc :: (KnownNat m, (n + m) ~ MicroLen) => MacroSteps n _ -> Microcode
mc ops = let (first, ops') = stepsOf ops
         in (first, ops' ++ pure (When Nothing, Nothing))

evalSrc src k = case src of
    Imm -> mc $ imm1 >++> k
    Op (Reg r) -> mc $ step @Nothing @Nothing (Get r) >:> k
    Op AddrHL -> mc $
        step @Nothing @Nothing (Get2 rHL) >:>
        step @(Just Indirect) @Nothing ReadMem >:>
        k

microcode :: Instr -> Microcode
microcode NOP = mc End
-- microcode HLT = mc _
microcode (INT b) = mc $ step @Nothing @Nothing (SetInt b) >:> End
microcode CMA = mc $
    step @Nothing @Nothing (Get rA) >:>
    step @Nothing @Nothing (Compute ConstFF SUB KeepC KeepA) >:>
    step @Nothing @Nothing (Set rA) >:>
    End
microcode CMC = mc $
    step @Nothing @Nothing (Compute0 fC Complement0) >:>
    End
microcode STC = mc $
    step @Nothing @Nothing (Compute0 fC ConstTrue0) >:>
    End
microcode (ALU fun src) = evalSrc src $
    step @Nothing @Nothing (Compute RegA fun SetC SetA) >:>
    step @Nothing @Nothing UpdateFlags >:>
    step @Nothing @Nothing (Set rA) >:>
    End
microcode (CMP src) = evalSrc src $
    step @Nothing @Nothing (Compute RegA SUB SetC SetA) >:>
    step @Nothing @Nothing UpdateFlags >:>
    End
microcode RRC = mc $ shiftRotate RotateR
microcode RLC = mc $ shiftRotate RotateL
microcode RAR = mc $ shiftRotate ShiftR
microcode RAL = mc $ shiftRotate ShiftL
microcode (RST irq) = mc $
    pushPC >++>
    step @Nothing @Nothing (Rst irq) >:>
    End
microcode JMP = mc $
    imm2 >++>
    step @Nothing @Nothing Jump >:>
    End
microcode (JMPIf cond) = mc $
    imm2 >++>
    step @Nothing @Nothing (When $ Just cond) >:>
    step @Nothing @Nothing Jump >:>
    End
microcode CALL = mc $
    imm2 >++>
    pushPC >++>
    step @Nothing @Nothing Jump >:>
    End
microcode (CALLIf cond) = mc $
    imm2 >++>
    step @Nothing @Nothing (When $ Just cond) >:>
    pushPC >++>
    step @Nothing @Nothing Jump >:>
    End
microcode RET = mc $
    pop2 >++>
    step @Nothing @Nothing Jump >:>
    End
microcode (RETIf cond) = mc $
    step @Nothing @Nothing (When $ Just cond) >:>
    pop2 >++>
    step @Nothing @Nothing Jump >:>
    End
microcode LDA = mc $
    imm2 >++>
    step @(Just Indirect) @Nothing ReadMem >:>
    step @Nothing         @Nothing (Set rA) >:>
    End
microcode STA = mc $
    imm2 >++>
    step @Nothing @(Just Indirect) (Get rA) >:>
    End
microcode (LDAX rp) = mc $
    step @Nothing         @Nothing (Get2 rp) >:>
    step @(Just Indirect) @Nothing ReadMem >:>
    step @Nothing         @Nothing (Set rA) >:>
    End
microcode (STAX rp) = mc $
    step @Nothing @Nothing         (Get2 rp) >:>
    step @Nothing @(Just Indirect) (Get rA) >:>
    End
microcode (DCX rp) = mc $
    step @Nothing @Nothing (Get2 rp) >:>
    step @Nothing @Nothing (Compute2 Dec2 KeepC) >:>
    step @Nothing @Nothing (Swap2 rp) >:>
    End
microcode (INX rp) = mc $
    step @Nothing @Nothing (Get2 rp) >:>
    step @Nothing @Nothing (Compute2 Inc2 KeepC) >:>
    step @Nothing @Nothing (Swap2 rp) >:>
    End
microcode (INR AddrHL) = mc $
    step @Nothing         @Nothing         (Get2 rHL) >:>
    step @(Just Indirect) @Nothing         ReadMem >:>
    step @Nothing         @Nothing         (Compute Const01 ADD KeepC SetA) >:>
    step @Nothing         @(Just Indirect) UpdateFlags >:>
    End
microcode (INR (Reg r)) = mc $
    step @Nothing @Nothing (Get r) >:>
    step @Nothing @Nothing (Compute Const01 ADD KeepC SetA) >:>
    step @Nothing @Nothing UpdateFlags >:>
    step @Nothing @Nothing (Set r) >:>
    End
microcode (DCR AddrHL) = mc $
    step @Nothing         @Nothing         (Get2 rHL) >:>
    step @(Just Indirect) @Nothing         ReadMem >:>
    step @Nothing         @Nothing         (Compute ConstFF ADD KeepC SetA) >:>
    step @Nothing         @(Just Indirect) UpdateFlags >:>
    End
microcode (DCR (Reg r)) = mc $
    step @Nothing @Nothing (Get r) >:>
    step @Nothing @Nothing (Compute ConstFF ADD KeepC SetA) >:>
    step @Nothing @Nothing UpdateFlags >:>
    step @Nothing @Nothing (Set r) >:>
    End
microcode (DAD rp) = mc $
    step @Nothing @Nothing (Get2 rp) >:>
    step @Nothing @Nothing (Compute2 AddHL SetC) >:>
    step @Nothing @Nothing (Swap2 rHL) >:>
    End
microcode DAA = mc $
    step @Nothing @Nothing (Get rA) >:>
    step @Nothing @Nothing FixupBCD >:>
    step @Nothing @Nothing UpdateFlags >:>
    step @Nothing @Nothing (Set rA) >:>
    End
microcode (LXI rp) = mc $
    imm2 >++>
    step @Nothing @Nothing (Swap2 rp) >:>
    End
microcode PCHL = mc $
    step @Nothing @Nothing (Get2 rHL) >:>
    step @Nothing @Nothing Jump >:>
    End
microcode SPHL = mc $
    step @Nothing @Nothing (Get2 rHL) >:>
    step @Nothing @Nothing (Swap2 SP) >:>
    End
microcode LHLD = mc $
    imm2 >++>
    step @(Just Indirect) @Nothing ReadMem >:>
    step @Nothing         @Nothing (Set rL) >:>
    step @Nothing         @Nothing (Compute2 Inc2 KeepC) >:>
    step @(Just Indirect) @Nothing ReadMem >:>
    step @Nothing         @Nothing (Set rH) >:>
    End
microcode SHLD = mc $
    imm2 >++>
    step @Nothing @(Just Indirect) (Get rL) >:>
    step @Nothing @Nothing         (Compute2 Inc2 KeepC) >:>
    step @Nothing @(Just Indirect) (Get rH) >:>
    End
microcode XTHL = mc $
    pop2 >++>
    step @Nothing @Nothing (Swap2 rHL) >:>
    push2
microcode (PUSH rp) = mc $
    step @Nothing @Nothing (Get2 rp) >:>
    push2
microcode (POP rp) = mc $
    pop2 >++>
    step @Nothing @Nothing (Swap2 rp) >:> End
microcode (MOV (Reg r) src) = evalSrc src $
    step @Nothing @Nothing (Set r) >:>
    End
microcode (MOV AddrHL src) = evalSrc src $
    step @Nothing @(Just Indirect) (Get2 rHL) >:>
    End
microcode XCHG = mc $
    step @Nothing @Nothing (Get2 rHL) >:>
    step @Nothing @Nothing (Swap2 rDE) >:>
    step @Nothing @Nothing (Swap2 rHL) >:>
    End
microcode IN = mc $
    step @(Just IncrPC) @Nothing ReadMem >:>
    step @Nothing       @Nothing (FromBuf AddrBuf) >:>
    step @(Just 'Port)  @Nothing ReadMem >:>
    step @Nothing       @Nothing (Set rA) >:>
    End
microcode OUT = mc $
    step @(Just IncrPC) @Nothing      ReadMem >:>
    step @Nothing       @Nothing      (FromBuf AddrBuf) >:>
    step @Nothing       @(Just 'Port) (Get rA) >:>
    End
-- -- microcode instr = errorX $ show instr
