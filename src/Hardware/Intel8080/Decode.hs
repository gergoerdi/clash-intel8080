{-# LANGUAGE ViewPatterns #-}
module Hardware.Intel8080.Decode (decodeInstr) where

import Prelude ()
import Data.Word
import Clash.Prelude
import Hardware.Intel8080
import Debug.Trace

decodeLHS :: Reg -> LHS
decodeLHS 6 = Addr RHL
decodeLHS reg = Reg reg

decodeCond :: Vec 3 Bit -> Cond
decodeCond cond = Cond flag b
  where
    (flag0, b) = unpack (pack cond) :: (Unsigned 2, Bool)
    flag = case flag0 of
        0b00 -> FZ
        0b01 -> FC
        0b10 -> FP
        0b11 -> FS

decodeRR :: Bit -> Bit -> RegPair
decodeRR 0 0 = RBC
decodeRR 0 1 = RDE
decodeRR 1 0 = RHL
decodeRR 1 1 = SP

decodeRRPP :: Bit -> Bit -> RegPair
decodeRRPP 1 1 = RAF
decodeRRPP r p = decodeRR r p

decodeInstr :: Unsigned 8 -> Instr
decodeInstr b1 =
    case b1 of
        $(bitPattern "01110110") -> HLT
        $(bitPattern "01......") -> MOV d (LHS s)
        $(bitPattern "00...110") -> MOV d Imm
        $(bitPattern "00..0001") -> LXI rr

        $(bitPattern "00111010") -> LDA
        $(bitPattern "00110010") -> STA
        $(bitPattern "00101010") -> LHLD
        $(bitPattern "00100010") -> SHLD
        $(bitPattern "00..1010") -> MOV (Reg RA) (LHS $ Addr rr) -- LDAX rp
        $(bitPattern "00..0010") -> MOV (Addr rr) (LHS $ Reg RA) -- STAX rp
        $(bitPattern "11101011") -> XCHG

        $(bitPattern "10000...") -> ALU (ADD False) (LHS s)
        $(bitPattern "11000110") -> ALU (ADD False) Imm
        $(bitPattern "10001...") -> ALU (ADD True)  (LHS s)
        $(bitPattern "11001110") -> ALU (ADD True)  Imm

        $(bitPattern "10010...") -> ALU (SUB False) (LHS s)
        $(bitPattern "11010110") -> ALU (SUB False) Imm
        $(bitPattern "10011...") -> ALU (SUB True)  (LHS s)
        $(bitPattern "11011110") -> ALU (SUB True)  Imm

        $(bitPattern "10100...") -> ALU AND (LHS s)
        $(bitPattern "11100110") -> ALU AND Imm

        $(bitPattern "10110...") -> ALU OR (LHS s)
        $(bitPattern "11110110") -> ALU OR Imm

        $(bitPattern "10101...") -> ALU XOR (LHS s)
        $(bitPattern "11101110") -> ALU XOR Imm

        $(bitPattern "10111...") -> CMP (LHS s)
        $(bitPattern "11111110") -> CMP Imm

        $(bitPattern "00...100") -> INR d
        $(bitPattern "00...101") -> DCR d
        $(bitPattern "00..0011") -> INX rr
        $(bitPattern "00..1011") -> DCX rr

        $(bitPattern "00..1001") -> DAD rr
        $(bitPattern "00100111") -> DAA

        $(bitPattern "00000111") -> SHROT $ Left  Rotate
        $(bitPattern "00001111") -> SHROT $ Right Rotate
        $(bitPattern "00010111") -> SHROT $ Left  Shift
        $(bitPattern "00011111") -> SHROT $ Right Shift

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
        $(bitPattern "11..0101") -> PUSH rrpp
        $(bitPattern "11..0001") -> POP rrpp
        $(bitPattern "11100011") -> XTHL
        $(bitPattern "11111001") -> SPHL

        $(bitPattern "11111011") -> INT True
        $(bitPattern "11110011") -> INT False
        $(bitPattern "11...111") -> RST $ bitCoerce (d2, d1, d0)
        $(bitPattern "00000000") -> NOP
        -- _ -> error $ printf "Unknown opcode: %02x" (fromIntegral b1 :: Word8)
        _ -> NOP

  where
    b1'@(_ :> _ :> d2@r :> d1@p :> d0 :> s2 :> s1 :> s0 :> Nil) = bitCoerce b1 :: Vec 8 Bit
    d = decodeLHS $ bitCoerce (d2, d1, d0)
    s = decodeLHS $ bitCoerce (s2, s1, s0)
    rr = decodeRR r p
    rrpp = decodeRRPP r p
    cond = decodeCond (d2 :> d1 :> d0 :> Nil)
