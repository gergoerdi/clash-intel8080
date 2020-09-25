module Hardware.Intel8080.ALU (binALU, shiftRotateALU) where

import Prelude ()
import Data.Word
import Clash.Prelude
import Hardware.Intel8080

binALU :: ALU -> Bool -> Value -> Value -> (Bool, Bool, Value)
binALU fun c x y = case fun of
    ADD c0 -> addC x y (c0 && c)
    SUB c0 -> subC x y (c0 && c)
    AND -> (testBit (x .|. y) 4, False, x .&. y)
    OR -> (False, False, x .|. y)
    XOR -> (False, False, x `xor` y)
  where
    addC x y c =
        let xl, yl, xh, yh, zl, zh :: Unsigned 4
            (xh, xl) = bitCoerce x
            (yh, yl) = bitCoerce y
            (a, zl) = bitCoerce $ (xl `add` yl) + if c then 1 else 0
            (c', zh) = bitCoerce $ (xh `add` yh) + if a then 1 else 0
        in (a, c', bitCoerce (zh, zl))
    subC x y c =
        let xl, yl, xh, yh, zl, zh :: Unsigned 4
            (xh, xl) = bitCoerce x
            (yh, yl) = bitCoerce y
            (a, zl) = bitCoerce $ (xl `sub` yl) - if c then 1 else 0
            (c', zh) = bitCoerce $ (xh `sub` yh) - if a then 1 else 0
        in (a, c', bitCoerce (zh, zl))

shiftRotateALU :: Either ShiftRotate ShiftRotate -> Bool -> Value -> (Bool, Value)
shiftRotateALU fun c x = case fun of
    Left sr -> (b7, bitCoerce (b654321, b0, case sr of Shift -> c; Rotate -> b7))
    Right sr -> (b0, bitCoerce (case sr of Shift -> c; Rotate -> b0, b7, b654321))
  where
    (b7, b654321, b0) = bitCoerce x :: (Bool, Unsigned 6, Bool)
