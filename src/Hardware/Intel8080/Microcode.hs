module Hardware.Intel8080.Microcode where

import Clash.Prelude

import Hardware.Intel8080
import Data.Steps
import Data.Wedge
import qualified Data.List as L
import Clash.Annotations.BitRepresentation hiding (Value)

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

{-# ANN module (DataReprAnn
                  $(liftQ [t|Wedge OutAddr InAddr|])
                  3
                  [ ConstrRepr 'Nowhere 0b111 0b011 []
                  , ConstrRepr 'Here    0b100 0b000 [0b011]
                  , ConstrRepr 'There   0b100 0b100 [0b011]
                  ]) #-}

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

imm2 :: MicroSteps True False
imm2 =
    step (IJust IncrPC) ToAddrBuf INothing >++>
    step (IJust IncrPC) ToAddrBuf INothing

push2 :: MicroSteps False False
push2 =
    step INothing FromAddrBuf (IJust DecrSP) >++>
    step INothing FromAddrBuf (IJust DecrSP) >++>
    step INothing nop         INothing

pushPC :: MicroSteps False True
pushPC =
    step INothing FromPC (IJust DecrSP) >++>
    step INothing FromPC (IJust DecrSP)

pop2 :: MicroSteps True False
pop2 =
    step (IJust IncrSP) ToAddrBuf INothing >++>
    step (IJust IncrSP) ToAddrBuf INothing

popPC :: MicroSteps True False
popPC = pop2 >++> step INothing Jump INothing

type MicroOp = (MicroInstr, Wedge OutAddr InAddr)
type Setup = Maybe InAddr
type MicroOps = [MicroOp]
type Microcode = (Setup, MicroOps)

padding :: SNat (p + 1) -> MicroSteps False False
padding = go . toUNat
  where
    go :: UNat (p + 1) -> MicroSteps False False
    go (USucc UZero) = step INothing nop INothing
    go (USucc m@(USucc _)) = step INothing nop INothing >++> go m

nop :: MicroInstr
nop = FromReg RA

padded
    :: Int
    -> MicroSteps pre False
    -> Microcode
padded n ops = (first, uops <> nops)
  where
    (first, uops) = stepsOf ops
    p = n - L.length uops - 1
    nops = L.replicate p (nop, Nowhere)

alu
    :: ALU
    -> RHS
    -> MicroInstr
    -> Microcode
alu fun rhs writeback = case rhs of
    Imm -> padded 7 $
        step (IJust IncrPC) (Compute RegA fun SetZSP SetAC SetC)  INothing >++>
        step INothing       writeback                             INothing
    LHS (Reg r) -> padded 4 $
        step INothing (FromReg r)                                 INothing >++>
        step INothing (Compute RegA fun SetZSP SetAC SetC)        INothing >++>
        step INothing writeback                                   INothing
    LHS (Addr rr) -> padded 7 $
        step INothing        (FromReg2 rr)                        INothing >++>
        step (IJust FromPtr) (Compute RegA fun SetZSP SetAC SetC) INothing >++>
        step INothing        writeback                            INothing

microcode :: Instr -> Microcode
microcode NOP = padded 4 $ step INothing nop INothing
microcode HLT = padded 7 $ step INothing Halt INothing
microcode (INT b) = padded 4 $ step INothing (SetInt b) INothing
microcode CMA = padded 4 $
    step INothing (FromReg RA)                                       INothing >++>
    step INothing (Compute ConstFF (Sub False) KeepZSP KeepAC KeepC) INothing >++>
    step INothing (ToReg RA)                                         INothing
microcode CMC = padded 4 $
    step INothing (Compute0 FC Complement) INothing
microcode STC = padded 4 $
    step INothing (Compute0 FC ConstTrue) INothing
microcode (ALU fun rhs) = alu fun rhs (ToReg RA)
microcode (CMP rhs) = alu (Sub False) rhs nop
microcode (SHROT sr) = padded 4 $
    step INothing (FromReg RA)   INothing >++>
    step INothing (ComputeSR sr) INothing >++>
    step INothing (ToReg RA)     INothing
microcode (RST irq) = padded 11 $
    pushPC >++>
    step INothing (Rst irq) INothing
microcode JMP = padded 10 $
    imm2 >++>
    step INothing Jump INothing
microcode (JMPIf cond) = padded 10 $
    imm2                               >++>
    padding (SNat @5)                  >++>
    step INothing (When cond) INothing >++>
    step INothing Jump        INothing
microcode CALL = padded 17 $
    imm2   >++>
    pushPC >++>
    step INothing Jump INothing
microcode (CALLIf cond) = padded 17 $
    padding (SNat @6)                  >++>
    imm2                               >++>
    step INothing (When cond) INothing >++>
    pushPC                             >++>
    step INothing Jump        INothing
microcode RET = padded 10 $
    popPC
microcode (RETIf cond) = padded 11 $
    padding (SNat @2)                  >++>
    step INothing (When cond) INothing >++>
    popPC
microcode LDA = padded 13 $
    imm2 >++>
    step (IJust FromPtr) (ToReg RA) INothing
microcode STA = padded 13 $
    imm2 >++>
    step INothing (FromReg RA) (IJust ToPtr) >++>
    step INothing nop          INothing
microcode (DCX rr) = padded 5 $
    step INothing (FromReg2 rr)  INothing >++>
    step INothing (Compute2 Dec) INothing >++>
    step INothing (SwapReg2 rr)  INothing
microcode (INX rr) = padded 5 $
    step INothing (FromReg2 rr)  INothing >++>
    step INothing (Compute2 Inc) INothing >++>
    step INothing (SwapReg2 rr)  INothing
microcode (DAD rr) = padded 10 $
    step INothing (FromReg2 rr)                                    INothing >++>
    step INothing (FromReg RL)                                     INothing >++>
    step INothing (Compute AddrLo (Add False) KeepZSP KeepAC SetC) INothing >++>
    step INothing ToAddrBuf                                        INothing >++>
    step INothing (FromReg RH)                                     INothing >++>
    step INothing (Compute AddrLo (Add True)  KeepZSP KeepAC SetC) INothing >++>
    step INothing ToAddrBuf                                        INothing >++>
    step INothing (SwapReg2 RHL)                                   INothing
microcode (INR (Addr rr)) = padded 10 $
    step INothing        (FromReg2 rr)                                    INothing      >++>
    step (IJust FromPtr) (Compute Const01 (Add False) SetZSP SetAC KeepC) (IJust ToPtr) >++>
    step INothing        nop                                              INothing
microcode (INR (Reg r)) = padded 5 $
    step INothing (FromReg r)                                      INothing >++>
    step INothing (Compute Const01 (Add False) SetZSP SetAC KeepC) INothing >++>
    step INothing (ToReg r)                                        INothing
microcode (DCR (Addr rr)) = padded 10 $
    step INothing        (FromReg2 rr)                                    INothing      >++>
    step (IJust FromPtr) (Compute ConstFF (Add False) SetZSP SetAC KeepC) (IJust ToPtr) >++>
    step INothing        nop                                              INothing
microcode (DCR (Reg r)) = padded 5 $
    step INothing (FromReg r)                                      INothing >++>
    step INothing (Compute ConstFF (Add False) SetZSP SetAC KeepC) INothing >++>
    step INothing (ToReg r)                                        INothing
microcode (LXI rr) = padded 10 $
    imm2 >++>
    step INothing (SwapReg2 rr) INothing
microcode PCHL = padded 5 $
    step INothing (FromReg2 RHL) INothing >++>
    step INothing Jump           INothing
microcode SPHL = padded 5 $
    step INothing (FromReg2 RHL) INothing >++>
    step INothing (SwapReg2 SP)  INothing
microcode LHLD = padded 16 $
    imm2 >++>
    step (IJust FromPtr) (ToReg RL)     INothing >++>
    step INothing        (Compute2 Inc) INothing >++>
    step (IJust FromPtr) (ToReg RH)     INothing
microcode SHLD = padded 16 $
    imm2 >++>
    step INothing (FromReg RL)   (IJust ToPtr) >++>
    step INothing (Compute2 Inc) INothing      >++>
    step INothing (FromReg RH)   (IJust ToPtr) >++>
    step INothing nop            INothing
microcode XTHL = padded 18 $
    pop2 >++>
    step INothing (SwapReg2 RHL) INothing >++>
    push2
microcode (PUSH rr) = padded 11 $
    step INothing (FromReg2 rr) INothing >++>
    push2
microcode (POP rr) = padded 10 $
    pop2 >++>
    step INothing (SwapReg2 rr) INothing
microcode (MOV (Reg r) src) = case src of
    Imm -> padded 7 $
        step (IJust IncrPC) (ToReg r) INothing
    LHS (Reg r') -> padded 5 $
        step INothing (FromReg r') INothing >++>
        step INothing (ToReg r)    INothing
    LHS (Addr rr) -> padded 7 $
        step INothing        (FromReg2 rr) INothing >++>
        step (IJust FromPtr) (ToReg r)     INothing
microcode (MOV (Addr rr) src) = case src of
    Imm -> padded 10 $
        step (IJust IncrPC) (FromReg2 rr) (IJust ToPtr) >++>
        step INothing       nop           INothing
    LHS (Reg r) -> padded 7 $
        step INothing (FromReg r)   INothing      >++>
        step INothing (FromReg2 rr) (IJust ToPtr) >++>
        step INothing nop           INothing
    LHS (Addr rr') -> padded 13 $
        step INothing        (FromReg2 rr') INothing      >++>
        step (IJust FromPtr) (FromReg2 rr)  (IJust ToPtr) >++>
        step INothing        nop            INothing
microcode XCHG = padded 4 $
    step INothing (FromReg2 RHL) INothing >++>
    step INothing (SwapReg2 RDE) INothing >++>
    step INothing (SwapReg2 RHL) INothing
microcode IN = padded 10 $
    step (IJust IncrPC)   ToAddrBuf  INothing >++>
    step (IJust FromPort) (ToReg RA) INothing
microcode OUT = padded 10 $
    step (IJust IncrPC) ToAddrBuf    INothing       >++>
    step INothing       (FromReg RA) (IJust ToPort) >++>
    step INothing       nop          INothing
-- microcode instr = errorX $ show instr
