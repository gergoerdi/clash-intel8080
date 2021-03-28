{-# LANGUAGE RankNTypes #-}
module Hardware.Intel8080.Microcode where

import Clash.Prelude

import Hardware.Intel8080
import Hardware.Intel8080.Steps
import Data.Wedge

data InAddr
    = FromPtr
    | FromPort
    | IncrPC
    | IncrSP
    deriving (Show, Eq, Enum, Bounded, Generic, NFDataX, Lift)

data OutAddr
    = ToPtr
    | ToPort
    | DecrSP
    deriving (Show, Eq, Enum, Bounded, Generic, NFDataX, Lift)

data UpdateAC
    = SetAC
    | KeepAC
    deriving (Show, Eq, Generic, NFDataX, Lift)

data UpdateC
    = SetC
    | KeepC
    deriving (Show, Eq, Generic, NFDataX, Lift)

data MicroInstr
    = FromReg Reg
    | ToReg Reg
    | FromPC
    | FromAddrBuf
    | ToAddrBuf
    | FromReg2 RegPair
    | SwapReg2 RegPair
    | Jump
    | When Cond
    | Compute ALUArg ALU UpdateAC UpdateC
    | ComputeSR (Either ShiftRotate ShiftRotate)
    | Compute2 ALU2
    | Compute0 Flag ALU0
    | UpdateFlags
    | Rst (Unsigned 3)
    | SetInt Bool
    | Halt
    deriving (Show, Generic, NFDataX, Lift)

data ALUArg
    = RegA
    | AddrLo
    | Const01
    | ConstFF
    deriving (Show, Generic, NFDataX, Lift)

data ALU2
    = Inc
    | Dec
    deriving (Show, Generic, NFDataX, Lift)

data ALU0
    = Complement
    | ConstTrue
    | ConstFalse
    deriving (Show, Generic, NFDataX, Lift)

type MicroSteps = Steps InAddr MicroInstr OutAddr

imm2 :: MicroSteps 2 True False
imm2 =
    step (IJust IncrPC) ToAddrBuf INothing >++>
    step (IJust IncrPC) ToAddrBuf INothing

push2 :: MicroSteps 3 False False
push2 =
    step INothing FromAddrBuf (IJust DecrSP) >++>
    step INothing FromAddrBuf (IJust DecrSP) >++>
    step INothing nop         INothing

pushPC :: MicroSteps 2 False True
pushPC =
    step INothing FromPC (IJust DecrSP) >++>
    step INothing FromPC (IJust DecrSP)

pop2 :: MicroSteps 2 True False
pop2 =
    step (IJust IncrSP) ToAddrBuf INothing >++>
    step (IJust IncrSP) ToAddrBuf INothing

popPC :: MicroSteps 3 True False
popPC = pop2 >++> step INothing Jump INothing

type MicroOp = (MicroInstr, Wedge OutAddr InAddr)
type MicroLen = 8
type Setup = Maybe InAddr
type MicroOps = Vec MicroLen MicroOp
type Microcode = (Setup, MicroOps)

nop :: MicroInstr
nop = FromReg RA

padded :: (KnownNat k, KnownNat n, ((n + 1) + k) ~ MicroLen) => MicroSteps (n + 1) pre False -> Microcode
padded ops = (first, uops ++ nops)
  where
    (first, uops) = stepsOf ops
    nops = repeat (nop, Nowhere)

withRHS
    :: (KnownNat n, KnownNat k, (n + 1 + k) ~ MicroLen)
    => (KnownNat k', (1 + n + 1 + k') ~ MicroLen)
    => RHS
    -> (forall pre. IMaybe pre InAddr -> MicroSteps (n + 1) pre False)
    -> Microcode
withRHS rhs k = case rhs of
    Imm -> padded $
        k (IJust IncrPC)
    LHS (Reg r) -> padded $
        step INothing (FromReg r) INothing >++>
        k INothing
    LHS (Addr rr) -> padded $
        step INothing (FromReg2 rr) INothing >++>
        k (IJust FromPtr)

microcode :: Instr -> Microcode
microcode NOP = padded $ step INothing nop INothing
microcode HLT = padded $ step INothing Halt INothing
microcode (INT b) = padded $ step INothing (SetInt b) INothing
microcode CMA = padded $
    step INothing (FromReg RA)                               INothing >++>
    step INothing (Compute ConstFF (Sub False) KeepAC KeepC) INothing >++>
    step INothing (ToReg RA)                                 INothing
microcode CMC = padded $
    step INothing (Compute0 FC Complement) INothing
microcode STC = padded $
    step INothing (Compute0 FC ConstTrue) INothing
microcode (ALU fun src) = withRHS src $ \read ->
    step read     (Compute RegA fun SetAC SetC) INothing >++>
    step INothing UpdateFlags                   INothing >++>
    step INothing (ToReg RA)                    INothing
microcode (CMP src) = withRHS src $ \read ->
    step read     (Compute RegA (Sub False) SetAC SetC) INothing >++>
    step INothing UpdateFlags                           INothing
microcode (SHROT sr) = padded $
    step INothing (FromReg RA)   INothing >++>
    step INothing (ComputeSR sr) INothing >++>
    step INothing (ToReg RA)     INothing
microcode (RST irq) = padded $
    pushPC >++>
    step INothing (Rst irq) INothing
microcode JMP = padded $
    imm2 >++>
    step INothing Jump INothing
microcode (JMPIf cond) = padded $
    imm2                               >++>
    step INothing (When cond) INothing >++>
    step INothing Jump        INothing
microcode CALL = padded $
    imm2   >++>
    pushPC >++>
    step INothing Jump INothing
microcode (CALLIf cond) = padded $
    imm2                               >++>
    step INothing (When cond) INothing >++>
    pushPC                             >++>
    step INothing Jump        INothing
microcode RET = padded $
    popPC
microcode (RETIf cond) = padded $
    step INothing (When cond) INothing >++>
    popPC
microcode LDA = padded $
    imm2 >++>
    step (IJust FromPtr) (ToReg RA) INothing
microcode STA = padded $
    imm2 >++>
    step INothing (FromReg RA) (IJust ToPtr) >++>
    step INothing nop          INothing
microcode (DCX rr) = padded $
    step INothing (FromReg2 rr)  INothing >++>
    step INothing (Compute2 Dec) INothing >++>
    step INothing (SwapReg2 rr)  INothing
microcode (INX rr) = padded $
    step INothing (FromReg2 rr)  INothing >++>
    step INothing (Compute2 Inc) INothing >++>
    step INothing (SwapReg2 rr)  INothing
microcode (DAD rr) = padded $
    step INothing (FromReg2 rr)                            INothing >++>
    step INothing (FromReg RL)                             INothing >++>
    step INothing (Compute AddrLo (Add False) KeepAC SetC) INothing >++>
    step INothing ToAddrBuf                                INothing >++>
    step INothing (FromReg RH)                             INothing >++>
    step INothing (Compute AddrLo (Add True)  KeepAC SetC) INothing >++>
    step INothing ToAddrBuf                                INothing >++>
    step INothing (SwapReg2 RHL)                           INothing
microcode (INR (Addr rr)) = padded $
    step INothing        (FromReg2 rr)                             INothing      >++>
    step (IJust FromPtr) (Compute Const01 (Add False) SetAC KeepC) INothing      >++>
    step INothing        UpdateFlags                               (IJust ToPtr) >++>
    step INothing        nop                                       INothing
microcode (INR (Reg r)) = padded $
    step INothing (FromReg r)                               INothing >++>
    step INothing (Compute Const01 (Add False) SetAC KeepC) INothing >++>
    step INothing UpdateFlags                               INothing >++>
    step INothing (ToReg r)                                 INothing
microcode (DCR (Addr rr)) = padded $
    step INothing        (FromReg2 rr)                             INothing      >++>
    step (IJust FromPtr) (Compute ConstFF (Add False) SetAC KeepC) INothing      >++>
    step INothing        UpdateFlags                               (IJust ToPtr) >++>
    step INothing        nop                                       INothing
microcode (DCR (Reg r)) = padded $
    step INothing (FromReg r)                               INothing >++>
    step INothing (Compute ConstFF (Add False) SetAC KeepC) INothing >++>
    step INothing UpdateFlags                               INothing >++>
    step INothing (ToReg r)                                 INothing
microcode (LXI rr) = padded $
    imm2 >++>
    step INothing (SwapReg2 rr) INothing
microcode PCHL = padded $
    step INothing (FromReg2 RHL) INothing >++>
    step INothing Jump           INothing
microcode SPHL = padded $
    step INothing (FromReg2 RHL) INothing >++>
    step INothing (SwapReg2 SP)  INothing
microcode LHLD = padded $
    imm2 >++>
    step (IJust FromPtr) (ToReg RL)     INothing >++>
    step INothing        (Compute2 Inc) INothing >++>
    step (IJust FromPtr) (ToReg RH)     INothing
microcode SHLD = padded $
    imm2 >++>
    step INothing (FromReg RL)   (IJust ToPtr) >++>
    step INothing (Compute2 Inc) INothing      >++>
    step INothing (FromReg RH)   (IJust ToPtr) >++>
    step INothing nop            INothing
microcode XTHL = padded $
    pop2 >++>
    step INothing (SwapReg2 RHL) INothing >++>
    push2
microcode (PUSH rr) = padded $
    step INothing (FromReg2 rr) INothing >++>
    push2
microcode (POP rr) = padded $
    pop2 >++>
    step INothing (SwapReg2 rr) INothing
microcode (MOV (Reg r) src) = withRHS src $ \read ->
    step read (ToReg r) INothing
microcode (MOV (Addr rr) src) = withRHS src $ \read ->
    step read     (FromReg2 rr) (IJust ToPtr) >++>
    step INothing nop           INothing
microcode XCHG = padded $
    step INothing (FromReg2 RHL) INothing >++>
    step INothing (SwapReg2 RDE) INothing >++>
    step INothing (SwapReg2 RHL) INothing
microcode IN = padded $
    step (IJust IncrPC)   ToAddrBuf  INothing >++>
    step (IJust FromPort) (ToReg RA) INothing
microcode OUT = padded $
    step (IJust IncrPC) ToAddrBuf    INothing       >++>
    step INothing       (FromReg RA) (IJust ToPort) >++>
    step INothing       nop          INothing
-- microcode instr = errorX $ show instr
