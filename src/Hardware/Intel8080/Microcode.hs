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
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFDataX, Lift)

data OutAddr
    = ToPtr
    | ToPort
    | DecrSP
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFDataX, Lift)

data UpdateZSP
    = SetZSP
    | KeepZSP
    deriving (Show, Eq, Ord, Generic, NFDataX, Lift)

data UpdateAC
    = SetAC
    | KeepAC
    deriving (Show, Eq, Ord, Generic, NFDataX, Lift)

data UpdateC
    = SetC
    | KeepC
    deriving (Show, Eq, Ord, Generic, NFDataX, Lift)

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
    | Compute ALUArg ALU UpdateZSP UpdateAC UpdateC
    | ComputeSR (Either ShiftRotate ShiftRotate)
    | Compute2 ALU2
    | Compute0 Flag ALU0
    | Rst (Unsigned 3)
    | SetInt Bool
    | Halt
    deriving (Show, Eq, Ord, Generic, NFDataX, Lift)

data ALUArg
    = RegA
    | AddrLo
    | Const01
    | ConstFF
    deriving (Show, Eq, Ord, Generic, NFDataX, Lift)

data ALU2
    = Inc
    | Dec
    deriving (Show, Eq, Ord, Generic, NFDataX, Lift)

data ALU0
    = Complement
    | ConstTrue
    deriving (Show, Eq, Ord, Generic, NFDataX, Lift)

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

type MicroOp = ((MicroInstr, Wedge OutAddr InAddr), Bool)
type MicroLen = 17
type Setup = Maybe InAddr
type MicroOps = Vec MicroLen MicroOp
type Microcode = (Setup, MicroOps)

padding :: SNat (p + 1) -> MicroSteps (p + 1) False False
padding = go . toUNat
  where
    go :: UNat (p + 1) -> MicroSteps (p + 1) False False
    go (USucc UZero) = step INothing nop INothing
    go (USucc m@(USucc _)) = step INothing nop INothing >++> go m

nop :: MicroInstr
nop = FromReg RA

padded
    :: forall k n p pre post. (KnownNat k, KnownNat p, (n + 1 + p + k) ~ MicroLen)
    => SNat (n + 1 + p + 1)
    -> MicroSteps (n + 1) pre False
    -> Microcode
padded SNat ops = (first, withCont (uops ++ nops1) ++ nops2)
  where
    (first, uops) = stepsOf ops
    nops1 = replicate (SNat @p) (nop, Nowhere)
    nops2 = repeat ((nop, Nowhere), False)

withCont :: (KnownNat n) => Vec (n + 1) a -> Vec (n + 1) (a, Bool)
withCont xs = zip xs $ repeat True :< False

alu
    :: ALU
    -> RHS
    -> MicroInstr
    -> Microcode
alu fun rhs writeback = case rhs of
    Imm -> padded (SNat @7) $
        step (IJust IncrPC) (Compute RegA fun SetZSP SetAC SetC)  INothing >++>
        step INothing       writeback                             INothing
    LHS (Reg r) -> padded (SNat @4) $
        step INothing (FromReg r)                                 INothing >++>
        step INothing (Compute RegA fun SetZSP SetAC SetC)        INothing >++>
        step INothing writeback                                   INothing
    LHS (Addr rr) -> padded (SNat @7) $
        step INothing        (FromReg2 rr)                        INothing >++>
        step (IJust FromPtr) (Compute RegA fun SetZSP SetAC SetC) INothing >++>
        step INothing        writeback                            INothing

microcode :: Instr -> Microcode
microcode NOP = padded (SNat @4) $ step INothing nop INothing
microcode HLT = padded (SNat @7) $ step INothing Halt INothing
microcode (INT b) = padded (SNat @4) $ step INothing (SetInt b) INothing
microcode CMA = padded (SNat @4) $
    step INothing (FromReg RA)                                       INothing >++>
    step INothing (Compute ConstFF (Sub False) KeepZSP KeepAC KeepC) INothing >++>
    step INothing (ToReg RA)                                         INothing
microcode CMC = padded (SNat @4) $
    step INothing (Compute0 FC Complement) INothing
microcode STC = padded (SNat @4) $
    step INothing (Compute0 FC ConstTrue) INothing
microcode (ALU fun rhs) = alu fun rhs (ToReg RA)
microcode (CMP rhs) = alu (Sub False) rhs nop
microcode (SHROT sr) = padded (SNat @4) $
    step INothing (FromReg RA)   INothing >++>
    step INothing (ComputeSR sr) INothing >++>
    step INothing (ToReg RA)     INothing
microcode (RST irq) = padded (SNat @11) $
    pushPC >++>
    step INothing (Rst irq) INothing
microcode JMP = padded (SNat @10) $
    imm2 >++>
    step INothing Jump INothing
microcode (JMPIf cond) = padded (SNat @10) $
    imm2                               >++>
    padding (SNat @5)                  >++>
    step INothing (When cond) INothing >++>
    step INothing Jump        INothing
microcode CALL = padded (SNat @17) $
    imm2   >++>
    pushPC >++>
    step INothing Jump INothing
microcode (CALLIf cond) = padded (SNat @17) $
    padding (SNat @6)                  >++>
    imm2                               >++>
    step INothing (When cond) INothing >++>
    pushPC                             >++>
    step INothing Jump        INothing
microcode RET = padded (SNat @10) $
    popPC
microcode (RETIf cond) = padded (SNat @11) $
    padding (SNat @2)                  >++>
    step INothing (When cond) INothing >++>
    popPC
microcode LDA = padded (SNat @13) $
    imm2 >++>
    step (IJust FromPtr) (ToReg RA) INothing
microcode STA = padded (SNat @13) $
    imm2 >++>
    step INothing (FromReg RA) (IJust ToPtr) >++>
    step INothing nop          INothing
microcode (DCX rr) = padded (SNat @5) $
    step INothing (FromReg2 rr)  INothing >++>
    step INothing (Compute2 Dec) INothing >++>
    step INothing (SwapReg2 rr)  INothing
microcode (INX rr) = padded (SNat @5) $
    step INothing (FromReg2 rr)  INothing >++>
    step INothing (Compute2 Inc) INothing >++>
    step INothing (SwapReg2 rr)  INothing
microcode (DAD rr) = padded (SNat @10) $
    step INothing (FromReg2 rr)                                    INothing >++>
    step INothing (FromReg RL)                                     INothing >++>
    step INothing (Compute AddrLo (Add False) KeepZSP KeepAC SetC) INothing >++>
    step INothing ToAddrBuf                                        INothing >++>
    step INothing (FromReg RH)                                     INothing >++>
    step INothing (Compute AddrLo (Add True)  KeepZSP KeepAC SetC) INothing >++>
    step INothing ToAddrBuf                                        INothing >++>
    step INothing (SwapReg2 RHL)                                   INothing
microcode (INR (Addr rr)) = padded (SNat @10) $
    step INothing        (FromReg2 rr)                                    INothing      >++>
    step (IJust FromPtr) (Compute Const01 (Add False) SetZSP SetAC KeepC) (IJust ToPtr) >++>
    step INothing        nop                                              INothing
microcode (INR (Reg r)) = padded (SNat @5) $
    step INothing (FromReg r)                                      INothing >++>
    step INothing (Compute Const01 (Add False) SetZSP SetAC KeepC) INothing >++>
    step INothing (ToReg r)                                        INothing
microcode (DCR (Addr rr)) = padded (SNat @10) $
    step INothing        (FromReg2 rr)                                    INothing      >++>
    step (IJust FromPtr) (Compute ConstFF (Add False) SetZSP SetAC KeepC) (IJust ToPtr) >++>
    step INothing        nop                                              INothing
microcode (DCR (Reg r)) = padded (SNat @5) $
    step INothing (FromReg r)                                      INothing >++>
    step INothing (Compute ConstFF (Add False) SetZSP SetAC KeepC) INothing >++>
    step INothing (ToReg r)                                        INothing
microcode (LXI rr) = padded (SNat @10) $
    imm2 >++>
    step INothing (SwapReg2 rr) INothing
microcode PCHL = padded (SNat @5) $
    step INothing (FromReg2 RHL) INothing >++>
    step INothing Jump           INothing
microcode SPHL = padded (SNat @5) $
    step INothing (FromReg2 RHL) INothing >++>
    step INothing (SwapReg2 SP)  INothing
microcode LHLD = padded (SNat @16) $
    imm2 >++>
    step (IJust FromPtr) (ToReg RL)     INothing >++>
    step INothing        (Compute2 Inc) INothing >++>
    step (IJust FromPtr) (ToReg RH)     INothing
microcode SHLD = padded (SNat @16) $
    imm2 >++>
    step INothing (FromReg RL)   (IJust ToPtr) >++>
    step INothing (Compute2 Inc) INothing      >++>
    step INothing (FromReg RH)   (IJust ToPtr) >++>
    step INothing nop            INothing
microcode XTHL = padded (SNat @18) $
    pop2 >++>
    step INothing (SwapReg2 RHL) INothing >++>
    push2
microcode (PUSH rr) = padded (SNat @11) $
    step INothing (FromReg2 rr) INothing >++>
    push2
microcode (POP rr) = padded (SNat @10) $
    pop2 >++>
    step INothing (SwapReg2 rr) INothing
microcode (MOV (Reg r) src) = case src of
    Imm -> padded (SNat @7) $
        step (IJust IncrPC) (ToReg r) INothing
    LHS (Reg r') -> padded (SNat @5) $
        step INothing (FromReg r') INothing >++>
        step INothing (ToReg r)    INothing
    LHS (Addr rr) -> padded (SNat @7) $
        step INothing        (FromReg2 rr) INothing >++>
        step (IJust FromPtr) (ToReg r)     INothing
microcode (MOV (Addr rr) src) = case src of
    Imm -> padded (SNat @10) $
        step (IJust IncrPC) (FromReg2 rr) (IJust ToPtr) >++>
        step INothing       nop           INothing
    LHS (Reg r) -> padded (SNat @7) $
        step INothing (FromReg r)   INothing      >++>
        step INothing (FromReg2 rr) (IJust ToPtr) >++>
        step INothing nop           INothing
    LHS (Addr rr') -> padded (SNat @13) $
        step INothing        (FromReg2 rr') INothing      >++>
        step (IJust FromPtr) (FromReg2 rr)  (IJust ToPtr) >++>
        step INothing        nop            INothing
microcode XCHG = padded (SNat @4) $
    step INothing (FromReg2 RHL) INothing >++>
    step INothing (SwapReg2 RDE) INothing >++>
    step INothing (SwapReg2 RHL) INothing
microcode IN = padded (SNat @10) $
    step (IJust IncrPC)   ToAddrBuf  INothing >++>
    step (IJust FromPort) (ToReg RA) INothing
microcode OUT = padded (SNat @10) $
    step (IJust IncrPC) ToAddrBuf    INothing       >++>
    step INothing       (FromReg RA) (IJust ToPort) >++>
    step INothing       nop          INothing
-- microcode instr = errorX $ show instr
