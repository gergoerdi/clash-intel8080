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

type MacroSteps ends n = Amble ends n Effect

imm1 = Step @(Just IncrPC) @Nothing ReadMem >:> End

imm2 =
    Step @(Just IncrPC) @Nothing ReadMem >:>
    Step @Nothing       @Nothing (FromBuf AddrBuf) >:>
    Step @(Just IncrPC) @Nothing ReadMem >:>
    Step @Nothing       @Nothing (FromBuf AddrBuf) >:>
    End

push2 =
    Step @Nothing @(Just DecrSP) (ToBuf AddrBuf) >:>
    Step @Nothing @(Just DecrSP) (ToBuf AddrBuf) >:>
    End

pushPC =
    Step @Nothing @(Just DecrSP) (ToBuf PC) >:>
    Step @Nothing @(Just DecrSP) (ToBuf PC) >:>
    End

pop2 =
    Step @(Just IncrSP) @Nothing ReadMem >:>
    Step @Nothing       @Nothing (FromBuf AddrBuf) >:>
    Step @(Just IncrSP) @Nothing ReadMem >:>
    Step @Nothing       @Nothing (FromBuf AddrBuf) >:>
    End

shiftRotate op =
    Step @Nothing @Nothing (Get rA) >:>
    Step @Nothing @Nothing (Compute RegA op SetC KeepA) >:>
    Step @Nothing @Nothing (Set rA) >:>
    End

type RW = Maybe (Either Addressing Addressing)
type MicroOp = (Effect, RW)
type MicroLen = 10
type Microcode = (Maybe Addressing, Vec MicroLen MicroOp)

mc :: (KnownNat m, (n + m) ~ MicroLen) => MacroSteps _ n -> Microcode
mc ops = let (first, ops') = stepsOf ops
         in (first, ops' ++ pure (When Nothing, Nothing))

evalSrc src k = case src of
    Imm -> mc $ imm1 >++> k
    Op (Reg r) -> mc $ Step @Nothing @Nothing (Get r) >:> k
    Op AddrHL -> mc $
        Step @Nothing @Nothing (Get2 rHL) >:>
        Step @(Just Indirect) @Nothing ReadMem >:>
        k

microcode :: Instr -> Microcode
microcode NOP = mc End
-- microcode HLT = mc _
microcode (INT b) = mc $ Step @Nothing @Nothing (SetInt b) >:> End
microcode CMA = mc $
    Step @Nothing @Nothing (Get rA) >:>
    Step @Nothing @Nothing (Compute ConstFF SUB KeepC KeepA) >:>
    Step @Nothing @Nothing (Set rA) >:>
    End
microcode CMC = mc $
    Step @Nothing @Nothing (Compute0 fC Complement0) >:>
    End
microcode STC = mc $
    Step @Nothing @Nothing (Compute0 fC ConstTrue0) >:>
    End
microcode (ALU fun src) = evalSrc src $
    Step @Nothing @Nothing (Compute RegA fun SetC SetA) >:>
    Step @Nothing @Nothing UpdateFlags >:>
    Step @Nothing @Nothing (Set rA) >:>
    End
microcode (CMP src) = evalSrc src $
    Step @Nothing @Nothing (Compute RegA SUB SetC SetA) >:>
    Step @Nothing @Nothing UpdateFlags >:>
    End
microcode RRC = mc $ shiftRotate RotateR
microcode RLC = mc $ shiftRotate RotateL
microcode RAR = mc $ shiftRotate ShiftR
microcode RAL = mc $ shiftRotate ShiftL
microcode (RST irq) = mc $
    pushPC >++>
    Step @Nothing @Nothing (Rst irq) >:>
    End
microcode JMP = mc $
    imm2 >++>
    Step @Nothing @Nothing Jump >:>
    End
microcode (JMPIf cond) = mc $
    imm2 >++>
    Step @Nothing @Nothing (When $ Just cond) >:>
    Step @Nothing @Nothing Jump >:>
    End
microcode CALL = mc $
    imm2 >++>
    pushPC >++>
    Step @Nothing @Nothing Jump >:>
    End
microcode (CALLIf cond) = mc $
    imm2 >++>
    Step @Nothing @Nothing (When $ Just cond) >:>
    pushPC >++>
    Step @Nothing @Nothing Jump >:>
    End
microcode RET = mc $
    pop2 >++>
    Step @Nothing @Nothing Jump >:>
    End
microcode (RETIf cond) = mc $
    Step @Nothing @Nothing (When $ Just cond) >:>
    pop2 >++>
    Step @Nothing @Nothing Jump >:>
    End
microcode LDA = mc $
    imm2 >++>
    Step @(Just Indirect) @Nothing ReadMem >:>
    Step @Nothing         @Nothing (Set rA) >:>
    End
microcode STA = mc $
    imm2 >++>
    Step @Nothing @(Just Indirect) (Get rA) >:>
    End
microcode (LDAX rp) = mc $
    Step @Nothing         @Nothing (Get2 rp) >:>
    Step @(Just Indirect) @Nothing ReadMem >:>
    Step @Nothing         @Nothing (Set rA) >:>
    End
microcode (STAX rp) = mc $
    Step @Nothing @Nothing         (Get2 rp) >:>
    Step @Nothing @(Just Indirect) (Get rA) >:>
    End
microcode (DCX rp) = mc $
    Step @Nothing @Nothing (Get2 rp) >:>
    Step @Nothing @Nothing (Compute2 Dec2 KeepC) >:>
    Step @Nothing @Nothing (Swap2 rp) >:>
    End
microcode (INX rp) = mc $
    Step @Nothing @Nothing (Get2 rp) >:>
    Step @Nothing @Nothing (Compute2 Inc2 KeepC) >:>
    Step @Nothing @Nothing (Swap2 rp) >:>
    End
microcode (INR AddrHL) = mc $
    Step @Nothing         @Nothing         (Get2 rHL) >:>
    Step @(Just Indirect) @Nothing         ReadMem >:>
    Step @Nothing         @Nothing         (Compute Const01 ADD KeepC SetA) >:>
    Step @Nothing         @(Just Indirect) UpdateFlags >:>
    End
microcode (INR (Reg r)) = mc $
    Step @Nothing @Nothing (Get r) >:>
    Step @Nothing @Nothing (Compute Const01 ADD KeepC SetA) >:>
    Step @Nothing @Nothing UpdateFlags >:>
    Step @Nothing @Nothing (Set r) >:>
    End
microcode (DCR AddrHL) = mc $
    Step @Nothing         @Nothing         (Get2 rHL) >:>
    Step @(Just Indirect) @Nothing         ReadMem >:>
    Step @Nothing         @Nothing         (Compute ConstFF ADD KeepC SetA) >:>
    Step @Nothing         @(Just Indirect) UpdateFlags >:>
    End
microcode (DCR (Reg r)) = mc $
    Step @Nothing @Nothing (Get r) >:>
    Step @Nothing @Nothing (Compute ConstFF ADD KeepC SetA) >:>
    Step @Nothing @Nothing UpdateFlags >:>
    Step @Nothing @Nothing (Set r) >:>
    End
microcode (DAD rp) = mc $
    Step @Nothing @Nothing (Get2 rp) >:>
    Step @Nothing @Nothing (Compute2 AddHL SetC) >:>
    Step @Nothing @Nothing (Swap2 rHL) >:>
    End
microcode DAA = mc $
    Step @Nothing @Nothing (Get rA) >:>
    Step @Nothing @Nothing FixupBCD >:>
    Step @Nothing @Nothing UpdateFlags >:>
    Step @Nothing @Nothing (Set rA) >:>
    End
microcode (LXI rp) = mc $
    imm2 >++>
    Step @Nothing @Nothing (Swap2 rp) >:>
    End
microcode PCHL = mc $
    Step @Nothing @Nothing (Get2 rHL) >:>
    Step @Nothing @Nothing Jump >:>
    End
microcode SPHL = mc $
    Step @Nothing @Nothing (Get2 rHL) >:>
    Step @Nothing @Nothing (Swap2 SP) >:>
    End
microcode LHLD = mc $
    imm2 >++>
    Step @(Just Indirect) @Nothing ReadMem >:>
    Step @Nothing         @Nothing (Set rL) >:>
    Step @Nothing         @Nothing (Compute2 Inc2 KeepC) >:>
    Step @(Just Indirect) @Nothing ReadMem >:>
    Step @Nothing         @Nothing (Set rH) >:>
    End
microcode SHLD = mc $
    imm2 >++>
    Step @Nothing @(Just Indirect) (Get rL) >:>
    Step @Nothing @Nothing         (Compute2 Inc2 KeepC) >:>
    Step @Nothing @(Just Indirect) (Get rH) >:>
    End
microcode XTHL = mc $
    pop2 >++>
    Step @Nothing @Nothing (Swap2 rHL) >:>
    push2
microcode (PUSH rp) = mc $
    Step @Nothing @Nothing (Get2 rp) >:>
    push2
microcode (POP rp) = mc $
    pop2 >++>
    Step @Nothing @Nothing (Swap2 rp) >:> End
microcode (MOV (Reg r) src) = evalSrc src $
    Step @Nothing @Nothing (Set r) >:>
    End
microcode (MOV AddrHL src) = evalSrc src $
    Step @Nothing @(Just Indirect) (Get2 rHL) >:>
    End
microcode XCHG = mc $
    Step @Nothing @Nothing (Get2 rHL) >:>
    Step @Nothing @Nothing (Swap2 rDE) >:>
    Step @Nothing @Nothing (Swap2 rHL) >:>
    End
microcode IN = mc $
    Step @(Just IncrPC) @Nothing ReadMem >:>
    Step @Nothing       @Nothing (FromBuf AddrBuf) >:>
    Step @(Just 'Port)  @Nothing ReadMem >:>
    Step @Nothing       @Nothing (Set rA) >:>
    End
microcode OUT = mc $
    Step @(Just IncrPC) @Nothing      ReadMem >:>
    Step @Nothing       @Nothing      (FromBuf AddrBuf) >:>
    Step @Nothing       @(Just 'Port) (Get rA) >:>
    End
-- -- microcode instr = errorX $ show instr
