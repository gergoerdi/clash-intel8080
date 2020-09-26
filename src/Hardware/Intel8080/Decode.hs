{-# LANGUAGE ViewPatterns #-}
module Hardware.Intel8080.Decode (decodeInstr) where

import Prelude ()
import Data.Word
import Clash.Prelude
import Hardware.Intel8080
import Debug.Trace

decodeLHS :: BitVector 3 -> LHS
decodeLHS 0b110 = Addr RHL
decodeLHS reg   = Reg (bitCoerce reg)

decodeCond :: BitVector 3 -> Cond
decodeCond cond = Cond flag b
  where
    (flag0, b) = bitCoerce cond :: (BitVector 2, Bool)
    flag = case flag0 of
        0b00 -> FZ
        0b01 -> FC
        0b10 -> FP
        0b11 -> FS

decodeRR :: BitVector 2 -> RegPair
decodeRR 0b00 = RBC
decodeRR 0b01 = RDE
decodeRR 0b10 = RHL
decodeRR 0b11 = SP

pushPopRR :: RegPair -> RegPair
pushPopRR SP = RAF
pushPopRR rr = rr

decodeInstr :: Value -> Instr
decodeInstr b = case b of
    $(bitPattern "01110110") -> HLT
    $(bitPattern "01......") -> MOV dest src
    $(bitPattern "00...110") -> MVI dest
    $(bitPattern "00..0001") -> LXI rr

    $(bitPattern "00111010") -> LDA
    $(bitPattern "00110010") -> STA
    $(bitPattern "00101010") -> LHLD
    $(bitPattern "00100010") -> SHLD
    $(bitPattern "00..1010") -> LDAX rr
    $(bitPattern "00..0010") -> STAX rr
    $(bitPattern "11101011") -> XCHG

    $(bitPattern "10000...") -> ADD src
    $(bitPattern "11000110") -> ADD Imm
    $(bitPattern "10001...") -> ADC src
    $(bitPattern "11001110") -> ADC Imm

    $(bitPattern "10010...") -> SUB src
    $(bitPattern "11010110") -> SUB Imm
    $(bitPattern "10011...") -> SBC src
    $(bitPattern "11011110") -> SBC Imm

    $(bitPattern "10100...") -> AND src
    $(bitPattern "11100110") -> AND Imm

    $(bitPattern "10101...") -> XOR src
    $(bitPattern "11101110") -> XOR Imm

    $(bitPattern "10110...") -> ORA src
    $(bitPattern "11110110") -> ORA Imm

    $(bitPattern "10111...") -> CMP src
    $(bitPattern "11111110") -> CMP Imm

    $(bitPattern "00...100") -> INR dest
    $(bitPattern "00...101") -> DCR dest
    $(bitPattern "00..0011") -> INX rr
    $(bitPattern "00..1011") -> DCX rr

    $(bitPattern "00..1001") -> DAD rr
    $(bitPattern "00100111") -> DAA

    $(bitPattern "00000111") -> RLC
    $(bitPattern "00001111") -> RRC
    $(bitPattern "00010111") -> RAL
    $(bitPattern "00011111") -> RAR

    $(bitPattern "00101111") -> CMA
    $(bitPattern "00111111") -> CMC
    $(bitPattern "00110111") -> STC

    $(bitPattern "11000011") -> JMP
    $(bitPattern "11...010") -> JMPIf cond
    $(bitPattern "11001101") -> CALL
    $(bitPattern "11...100") -> CALLIf cond
    $(bitPattern "11001001") -> RET
    $(bitPattern "11...000") -> RETIf cond

    $(bitPattern "11011011") -> IN
    $(bitPattern "11010011") -> OUT

    $(bitPattern "11101001") -> PCHL
    $(bitPattern "11..0101") -> PUSH rr'
    $(bitPattern "11..0001") -> POP rr'
    $(bitPattern "11100011") -> XTHL
    $(bitPattern "11111001") -> SPHL

    $(bitPattern "11111011") -> EI
    $(bitPattern "11110011") -> DI
    $(bitPattern "11...111") -> RST (bitCoerce op1)
    $(bitPattern "00000000") -> NOP
    -- _ -> error $ printf "Unknown opcode: %02x" (fromIntegral b1 :: Word8)
    _ -> NOP
  where
    op1 = slice (SNat @5) (SNat @3) b
    op2 = slice (SNat @2) (SNat @0) b

    dest = decodeLHS op1
    src = LHS $ decodeLHS op2

    rr = decodeRR $ slice (SNat @5) (SNat @4) b
    rr' = pushPopRR rr
    cond = decodeCond op1
