module Hardware.Intel8080.Microcode where

import Clash.Prelude

import Hardware.Intel8080
import Hardware.Intel8080.Steps

data Addressing
    = Indirect
    | Port
    | IncrPC
    | IncrSP
    | DecrSP
    deriving (Show)

data UpdateAC
    = SetAC
    | KeepAC
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
    | Compute ALUArg ALU UpdateC UpdateAC
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

type MicroSteps = Steps Addressing Effect Addressing

imm1 = step (IJust IncrPC) ReadMem INothing

imm2 =
    step (IJust IncrPC) ReadMem           INothing >++>
    step INothing       (FromBuf AddrBuf) INothing >++>
    step (IJust IncrPC) ReadMem           INothing >++>
    step INothing       (FromBuf AddrBuf) INothing

push2 =
    step INothing (ToBuf AddrBuf) (IJust DecrSP) >++>
    step INothing (ToBuf AddrBuf) (IJust DecrSP)

pushPC =
    step INothing (ToBuf PC) (IJust DecrSP) >++>
    step INothing (ToBuf PC) (IJust DecrSP)

pop2 =
    step (IJust IncrSP) ReadMem           INothing >++>
    step INothing       (FromBuf AddrBuf) INothing >++>
    step (IJust IncrSP) ReadMem           INothing >++>
    step INothing       (FromBuf AddrBuf) INothing

shiftRotate op =
    step INothing (Get RA)                      INothing >++>
    step INothing (Compute RegA op SetC KeepAC) INothing >++>
    step INothing (Set RA)                      INothing

type RW = Maybe (Either Addressing Addressing)
type MicroOp = (Effect, RW)
type MicroLen = 10
type Microcode = (Maybe Addressing, Vec MicroLen MicroOp)

mc :: (KnownNat k, (1 + n + k) ~ MicroLen) => MicroSteps (1 + n) pre post -> Microcode
mc ops = let (first, ops') = stepsOf ops
         in (first, ops' ++ repeat (When Nothing, Nothing))

evalSrc src k = case src of
    Imm -> mc $ imm1 >++> k
    Op (Reg r) -> mc $
        step INothing (Get r) INothing >++>
        k
    Op (Addr rr) -> mc $
        step INothing         (Get2 rr) INothing >++>
        step (IJust Indirect) ReadMem   INothing >++>
        k

microcode :: Instr -> Microcode
microcode NOP = mc $ step INothing (When Nothing) INothing
-- microcode HLT = mc _
microcode (INT b) = mc $ step INothing (SetInt b) INothing
microcode CMA = mc $
    step INothing (Get RA)                           INothing >++>
    step INothing (Compute ConstFF SUB KeepC KeepAC) INothing >++>
    step INothing (Set RA)                           INothing
microcode CMC = mc $
    step INothing (Compute0 FC Complement0) INothing
microcode STC = mc $
    step INothing (Compute0 FC ConstTrue0) INothing
microcode (ALU fun src) = evalSrc src $
    step INothing (Compute RegA fun SetC SetAC) INothing >++>
    step INothing UpdateFlags                   INothing >++>
    step INothing (Set RA)                      INothing
microcode (CMP src) = evalSrc src $
    step INothing (Compute RegA SUB SetC SetAC) INothing >++>
    step INothing UpdateFlags                   INothing
microcode RRC = mc $ shiftRotate RotateR
microcode RLC = mc $ shiftRotate RotateL
microcode RAR = mc $ shiftRotate ShiftR
microcode RAL = mc $ shiftRotate ShiftL
microcode (RST irq) = mc $
    pushPC >++>
    step INothing (Rst irq) INothing
microcode JMP = mc $
    imm2 >++>
    step INothing Jump INothing
microcode (JMPIf cond) = mc $
    imm2 >++>
    step INothing (When $ Just cond) INothing >++>
    step INothing Jump               INothing
microcode CALL = mc $
    imm2 >++>
    pushPC >++>
    step INothing Jump INothing
microcode (CALLIf cond) = mc $
    imm2 >++>
    step INothing (When $ Just cond) INothing >++>
    pushPC                                    >++>
    step INothing Jump               INothing
microcode RET = mc $
    pop2 >++>
    step INothing Jump INothing
microcode (RETIf cond) = mc $
    step INothing (When $ Just cond) INothing >++>
    pop2 >++>
    step INothing Jump               INothing
microcode LDA = mc $
    imm2 >++>
    step (IJust Indirect) ReadMem  INothing >++>
    step INothing         (Set RA) INothing
microcode STA = mc $
    imm2 >++>
    step INothing (Get RA) (IJust Indirect)
microcode (DCX rr) = mc $
    step INothing (Get2 rr)             INothing >++>
    step INothing (Compute2 Dec2 KeepC) INothing >++>
    step INothing (Swap2 rr)            INothing
microcode (INX rr) = mc $
    step INothing (Get2 rr)             INothing >++>
    step INothing (Compute2 Inc2 KeepC) INothing >++>
    step INothing (Swap2 rr)            INothing
microcode (INR (Addr rr)) = mc $
    step INothing         (Get2 rr)                         INothing >++>
    step (IJust Indirect) ReadMem                           INothing >++>
    step INothing         (Compute Const01 ADD KeepC SetAC) INothing >++>
    step INothing         UpdateFlags                       (IJust Indirect)
microcode (INR (Reg r)) = mc $
    step INothing (Get r)                           INothing >++>
    step INothing (Compute Const01 ADD KeepC SetAC) INothing >++>
    step INothing UpdateFlags                       INothing >++>
    step INothing (Set r)                           INothing
microcode (DCR (Addr rr)) = mc $
    step INothing         (Get2 rr)                         INothing >++>
    step (IJust Indirect) ReadMem                           INothing >++>
    step INothing         (Compute ConstFF ADD KeepC SetAC) INothing >++>
    step INothing         UpdateFlags                       (IJust Indirect)
microcode (DCR (Reg r)) = mc $
    step INothing (Get r)                           INothing >++>
    step INothing (Compute ConstFF ADD KeepC SetAC) INothing >++>
    step INothing UpdateFlags                       INothing >++>
    step INothing (Set r)                           INothing
microcode (DAD rr) = mc $
    step INothing (Get2 rr)             INothing >++>
    step INothing (Compute2 AddHL SetC) INothing >++>
    step INothing (Swap2 RHL)           INothing
microcode DAA = mc $
    step INothing (Get RA)    INothing >++>
    step INothing FixupBCD    INothing >++>
    step INothing UpdateFlags INothing >++>
    step INothing (Set RA)    INothing
microcode (LXI rr) = mc $
    imm2 >++>
    step INothing (Swap2 rr) INothing
microcode PCHL = mc $
    step INothing (Get2 RHL) INothing >++>
    step INothing Jump       INothing
microcode SPHL = mc $
    step INothing (Get2 RHL) INothing >++>
    step INothing (Swap2 SP) INothing
microcode LHLD = mc $
    imm2 >++>
    step (IJust Indirect) ReadMem               INothing >++>
    step INothing         (Set RL)              INothing >++>
    step INothing         (Compute2 Inc2 KeepC) INothing >++>
    step (IJust Indirect) ReadMem               INothing >++>
    step INothing         (Set RH)              INothing
microcode SHLD = mc $
    imm2 >++>
    step INothing (Get RL)              (IJust Indirect) >++>
    step INothing (Compute2 Inc2 KeepC) INothing         >++>
    step INothing (Get RH)              (IJust Indirect)
microcode XTHL = mc $
    pop2 >++>
    step INothing (Swap2 RHL) INothing >++>
    push2
microcode (PUSH rr) = mc $
    step INothing (Get2 rr) INothing >++>
    push2
microcode (POP rr) = mc $
    pop2 >++>
    step INothing (Swap2 rr) INothing
microcode (MOV (Reg r) src) = evalSrc src $
    step INothing (Set r) INothing
microcode (MOV (Addr rr) src) = evalSrc src $
    step INothing (Get2 rr) (IJust Indirect)
microcode XCHG = mc $
    step INothing (Get2 RHL)  INothing >++>
    step INothing (Swap2 RDE) INothing >++>
    step INothing (Swap2 RHL) INothing
microcode IN = mc $
    imm1 >++>
    step INothing     (FromBuf AddrBuf) INothing >++>
    step (IJust Port) ReadMem           INothing >++>
    step INothing     (Set RA)          INothing
microcode OUT = mc $
    imm1 >++>
    step INothing (FromBuf AddrBuf) INothing      >++>
    step INothing (Get RA)          (IJust Port)
-- microcode instr = errorX $ show instr
