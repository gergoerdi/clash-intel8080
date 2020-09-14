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

imm1 = step @(Just IncrPC) ReadMem @Nothing >:> End

imm2 =
    step @(Just IncrPC) ReadMem           @Nothing >:>
    step @Nothing       (FromBuf AddrBuf) @Nothing >:>
    step @(Just IncrPC) ReadMem           @Nothing >:>
    step @Nothing       (FromBuf AddrBuf) @Nothing >:>
    End

push2 =
    step @Nothing (ToBuf AddrBuf) @(Just DecrSP) >:>
    step @Nothing (ToBuf AddrBuf) @(Just DecrSP) >:>
    End

pushPC =
    step @Nothing (ToBuf PC) @(Just DecrSP) >:>
    step @Nothing (ToBuf PC) @(Just DecrSP) >:>
    End

pop2 =
    step @(Just IncrSP) ReadMem           @Nothing >:>
    step @Nothing       (FromBuf AddrBuf) @Nothing >:>
    step @(Just IncrSP) ReadMem           @Nothing >:>
    step @Nothing       (FromBuf AddrBuf) @Nothing >:>
    End

shiftRotate op =
    step @Nothing (Get rA)                     @Nothing >:>
    step @Nothing (Compute RegA op SetC KeepA) @Nothing >:>
    step @Nothing (Set rA)                     @Nothing >:>
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
    Op (Reg r) -> mc $
        step @Nothing (Get r) @Nothing >:>
        k
    Op AddrHL -> mc $
        step @Nothing         (Get2 rHL) @Nothing >:>
        step @(Just Indirect) ReadMem    @Nothing >:>
        k

microcode :: Instr -> Microcode
microcode NOP = mc End
-- microcode HLT = mc _
microcode (INT b) = mc $ step @Nothing (SetInt b) @Nothing >:> End
microcode CMA = mc $
    step @Nothing (Get rA)                          @Nothing >:>
    step @Nothing (Compute ConstFF SUB KeepC KeepA) @Nothing >:>
    step @Nothing (Set rA)                          @Nothing >:>
    End
microcode CMC = mc $
    step @Nothing (Compute0 fC Complement0) @Nothing >:>
    End
microcode STC = mc $
    step @Nothing (Compute0 fC ConstTrue0) @Nothing >:>
    End
microcode (ALU fun src) = evalSrc src $
    step @Nothing (Compute RegA fun SetC SetA) @Nothing >:>
    step @Nothing UpdateFlags                  @Nothing >:>
    step @Nothing (Set rA)                     @Nothing >:>
    End
microcode (CMP src) = evalSrc src $
    step @Nothing (Compute RegA SUB SetC SetA) @Nothing >:>
    step @Nothing UpdateFlags                  @Nothing >:>
    End
microcode RRC = mc $ shiftRotate RotateR
microcode RLC = mc $ shiftRotate RotateL
microcode RAR = mc $ shiftRotate ShiftR
microcode RAL = mc $ shiftRotate ShiftL
microcode (RST irq) = mc $
    pushPC >++>
    step @Nothing (Rst irq) @Nothing >:>
    End
microcode JMP = mc $
    imm2 >++>
    step @Nothing Jump @Nothing >:>
    End
microcode (JMPIf cond) = mc $
    imm2 >++>
    step @Nothing (When $ Just cond) @Nothing >:>
    step @Nothing Jump               @Nothing >:>
    End
microcode CALL = mc $
    imm2 >++>
    pushPC >++>
    step @Nothing Jump @Nothing >:>
    End
microcode (CALLIf cond) = mc $
    imm2 >++>
    step @Nothing (When $ Just cond) @Nothing >:>
    pushPC >++>
    step @Nothing Jump               @Nothing >:>
    End
microcode RET = mc $
    pop2 >++>
    step @Nothing Jump @Nothing >:>
    End
microcode (RETIf cond) = mc $
    step @Nothing (When $ Just cond) @Nothing >:>
    pop2 >++>
    step @Nothing Jump               @Nothing >:>
    End
microcode LDA = mc $
    imm2 >++>
    step @(Just Indirect) ReadMem  @Nothing >:>
    step @Nothing         (Set rA) @Nothing >:>
    End
microcode STA = mc $
    imm2 >++>
    step @Nothing (Get rA) @(Just Indirect) >:>
    End
microcode (LDAX rp) = mc $
    step @Nothing         (Get2 rp) @Nothing >:>
    step @(Just Indirect) ReadMem   @Nothing >:>
    step @Nothing         (Set rA)  @Nothing >:>
    End
microcode (STAX rp) = mc $
    step @Nothing (Get2 rp) @Nothing >:>
    step @Nothing (Get rA)  @(Just Indirect) >:>
    End
microcode (DCX rp) = mc $
    step @Nothing (Get2 rp)             @Nothing >:>
    step @Nothing (Compute2 Dec2 KeepC) @Nothing >:>
    step @Nothing (Swap2 rp)            @Nothing >:>
    End
microcode (INX rp) = mc $
    step @Nothing (Get2 rp)             @Nothing >:>
    step @Nothing (Compute2 Inc2 KeepC) @Nothing >:>
    step @Nothing (Swap2 rp)            @Nothing >:>
    End
microcode (INR AddrHL) = mc $
    step @Nothing         (Get2 rHL)                       @Nothing >:>
    step @(Just Indirect) ReadMem                          @Nothing >:>
    step @Nothing         (Compute Const01 ADD KeepC SetA) @Nothing >:>
    step @Nothing         UpdateFlags                      @(Just Indirect) >:>
    End
microcode (INR (Reg r)) = mc $
    step @Nothing (Get r)                          @Nothing >:>
    step @Nothing (Compute Const01 ADD KeepC SetA) @Nothing >:>
    step @Nothing UpdateFlags                      @Nothing >:>
    step @Nothing (Set r)                          @Nothing >:>
    End
microcode (DCR AddrHL) = mc $
    step @Nothing         (Get2 rHL)                       @Nothing >:>
    step @(Just Indirect) ReadMem                          @Nothing >:>
    step @Nothing         (Compute ConstFF ADD KeepC SetA) @Nothing >:>
    step @Nothing         UpdateFlags                      @(Just Indirect) >:>
    End
microcode (DCR (Reg r)) = mc $
    step @Nothing (Get r)                          @Nothing >:>
    step @Nothing (Compute ConstFF ADD KeepC SetA) @Nothing >:>
    step @Nothing UpdateFlags                      @Nothing >:>
    step @Nothing (Set r)                          @Nothing >:>
    End
microcode (DAD rp) = mc $
    step @Nothing (Get2 rp)             @Nothing >:>
    step @Nothing (Compute2 AddHL SetC) @Nothing >:>
    step @Nothing (Swap2 rHL)           @Nothing >:>
    End
microcode DAA = mc $
    step @Nothing (Get rA)    @Nothing >:>
    step @Nothing FixupBCD    @Nothing >:>
    step @Nothing UpdateFlags @Nothing >:>
    step @Nothing (Set rA)    @Nothing >:>
    End
microcode (LXI rp) = mc $
    imm2 >++>
    step @Nothing (Swap2 rp) @Nothing >:>
    End
microcode PCHL = mc $
    step @Nothing (Get2 rHL) @Nothing >:>
    step @Nothing Jump       @Nothing >:>
    End
microcode SPHL = mc $
    step @Nothing (Get2 rHL) @Nothing >:>
    step @Nothing (Swap2 SP) @Nothing >:>
    End
microcode LHLD = mc $
    imm2 >++>
    step @(Just Indirect) ReadMem               @Nothing >:>
    step @Nothing         (Set rL)              @Nothing >:>
    step @Nothing         (Compute2 Inc2 KeepC) @Nothing >:>
    step @(Just Indirect) ReadMem               @Nothing >:>
    step @Nothing         (Set rH)              @Nothing >:>
    End
microcode SHLD = mc $
    imm2 >++>
    step @Nothing (Get rL)              @(Just Indirect) >:>
    step @Nothing (Compute2 Inc2 KeepC) @Nothing         >:>
    step @Nothing (Get rH)              @(Just Indirect) >:>
    End
microcode XTHL = mc $
    pop2 >++>
    step @Nothing (Swap2 rHL) @Nothing >:>
    push2
microcode (PUSH rp) = mc $
    step @Nothing (Get2 rp) @Nothing >:>
    push2
microcode (POP rp) = mc $
    pop2 >++>
    step @Nothing (Swap2 rp) @Nothing >:> End
microcode (MOV (Reg r) src) = evalSrc src $
    step @Nothing (Set r) @Nothing >:>
    End
microcode (MOV AddrHL src) = evalSrc src $
    step @Nothing (Get2 rHL) @(Just Indirect) >:>
    End
microcode XCHG = mc $
    step @Nothing (Get2 rHL)  @Nothing >:>
    step @Nothing (Swap2 rDE) @Nothing >:>
    step @Nothing (Swap2 rHL) @Nothing >:>
    End
microcode IN = mc $
    imm1                                           >++>
    step @Nothing       (FromBuf AddrBuf) @Nothing >:>
    step @(Just 'Port)  ReadMem           @Nothing >:>
    step @Nothing       (Set rA)          @Nothing >:>
    End
microcode OUT = mc $
    imm1                                                >++>
    step @Nothing       (FromBuf AddrBuf) @Nothing      >:>
    step @Nothing       (Get rA)          @(Just 'Port) >:>
    End
-- -- microcode instr = errorX $ show instr
