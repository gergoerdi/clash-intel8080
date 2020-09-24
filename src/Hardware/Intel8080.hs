{-# LANGUAGE PatternSynonyms #-}
module Hardware.Intel8080 where

import Prelude ()
-- import Data.Word
import Clash.Prelude

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

data RegPair = Regs Reg Reg | SP
    deriving (Eq, Ord, Show, Generic, NFDataX)

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

data Op
    = Reg Reg
    | Addr RegPair
    deriving (Eq, Ord, Show, Generic, NFDataX)

type Value = Unsigned 8
type Addr = Unsigned 16
type Port = Unsigned 8
type Interrupt = Unsigned 3

data Src
    = Op Op
    | Imm
    deriving (Eq, Ord, Show, Generic, NFDataX)

data ALU = ADD | ADC | SUB | SBB | AND | OR | XOR | RotateR | RotateL | ShiftR | ShiftL
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, NFDataX)

data Cond = Cond Flag Bool
    deriving (Eq, Ord, Show, Generic, NFDataX)

data Instr
    = MOV Op Src
    | LXI RegPair
    | LDA
    | STA
    | LHLD
    | SHLD
    | XCHG
    | ALU ALU Src
    | CMP Src
    | INR Op
    | DCR Op
    | INX RegPair
    | DCX RegPair
    | DAD RegPair
    | DAA
    | RLC
    | RRC
    | RAL
    | RAR
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
