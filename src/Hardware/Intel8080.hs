{-# LANGUAGE PatternSynonyms #-}
module Hardware.Intel8080 where

import Clash.Prelude
import Clash.Annotations.BitRepresentation hiding (Value)

type Reg = Index 8

pattern RA, RFlags, RB, RC, RD, RE, RH, RL :: Reg
pattern RA = 7
pattern RFlags = 6
pattern RB = 0
pattern RC = 1
pattern RD = 2
pattern RE = 3
pattern RH = 4
pattern RL = 5

data RegPair
    = Regs Reg Reg
    | SP
    deriving (Eq, Ord, Show, Generic, NFDataX, Lift)

pattern RAF, RBC, RDE, RHL :: RegPair
pattern RAF = Regs RA RFlags
pattern RBC = Regs RB RC
pattern RDE = Regs RD RE
pattern RHL = Regs RH RL

type Flag = Index (BitSize Value)

pattern FS, FZ, FAC, FP, FC :: Flag
pattern FS = 7
pattern FZ = 6
pattern FAC = 4
pattern FP = 2
pattern FC = 0

type Value = Unsigned 8
type Addr = Unsigned 16
type Port = Unsigned 8
type Interrupt = Unsigned 3

data LHS
    = Reg Reg
    | Addr RegPair
    deriving (Eq, Ord, Show, Generic, NFDataX)

data RHS
    = Imm
    | LHS LHS
    deriving (Eq, Ord, Show, Generic, NFDataX)

data ALU = Add Bool | Sub Bool | And | XOr | Or | BCD
    deriving (Eq, Ord, Show, Generic, NFDataX, Lift)

{-# ANN module (DataReprAnn
                  $(liftQ [t|ALU|])
                  3
                  [ ConstrRepr 'Add 0b110 0b000 [0b001]
                  , ConstrRepr 'Sub 0b110 0b010 [0b001]
                  , ConstrRepr 'And 0b111 0b100 []
                  , ConstrRepr 'XOr 0b111 0b101 []
                  , ConstrRepr 'Or  0b111 0b110 []
                  , ConstrRepr 'BCD 0b111 0b111 []
                  ]) #-}

data ShiftRotate = Shift | Rotate
    deriving (Eq, Ord, Show, Generic, NFDataX, Lift)

data Cond = Cond Flag Bool
    deriving (Eq, Ord, Show, Generic, NFDataX, Lift)

data Instr
    = MOV LHS RHS
    | LXI RegPair
    | LDA
    | STA
    | LHLD
    | SHLD
    | XCHG
    | ALU ALU RHS
    | CMP RHS
    | SHROT (Either ShiftRotate ShiftRotate)
    | INR LHS
    | DCR LHS
    | INX RegPair
    | DCX RegPair
    | DAD RegPair
    | CMA
    | CMC
    | STC
    | JMP
    | JMPIf Cond
    | CALL
    | CALLIf Cond
    | RET
    | RETIf Cond
    | RST (Unsigned 3)
    | PCHL
    | PUSH RegPair
    | POP RegPair
    | XTHL
    | SPHL
    | IN
    | OUT
    | INT Bool
    | HLT
    | NOP
    deriving (Eq, Ord, Show, Generic, NFDataX)

pattern MVI lhs = MOV lhs Imm

pattern ADD rhs = ALU (Add False) rhs
pattern ADC rhs = ALU (Add True) rhs
pattern SUB rhs = ALU (Sub False) rhs
pattern SBC rhs = ALU (Sub True) rhs
pattern AND rhs = ALU And rhs
pattern ORA rhs = ALU Or rhs
pattern XOR rhs = ALU XOr rhs
pattern DAA = ALU BCD (LHS (Reg RA))
pattern LDAX rr = MOV (Reg RA) (LHS (Addr rr))
pattern STAX rr = MOV (Addr rr) (LHS (Reg RA))

pattern RLC = SHROT (Left Rotate)
pattern RRC = SHROT (Right Rotate)
pattern RAL = SHROT (Left Shift)
pattern RAR = SHROT (Right Shift)

pattern DI = INT False
pattern EI = INT True
