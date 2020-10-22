module Hardware.Intel8080.ALU (binALU, shiftRotateALU) where

import Prelude ()
import Data.Word
import Clash.Prelude
import Hardware.Intel8080

nybbles :: Value -> (Unsigned 4, Unsigned 4)
nybbles = bitCoerce

byte :: (Unsigned 4, Unsigned 4) -> Value
byte = bitCoerce

binALU :: ALU -> Value -> (Bool, Bool, Value) -> (Bool, Bool, Value)
binALU fun x (a, c, y) = case fun of
    Add c0 -> addC (c0 && c)
    Sub c0 -> subC (c0 && c)
    And    -> (testBit (x .|. y) 4, False, x .&. y)
    Or     -> (False, False, x .|. y)
    XOr    -> (False, False, x `xor` y)
    BCD    -> bcd
  where
    (xh, xl) = nybbles x
    (yh, yl) = nybbles y

    addC c = (a', c', byte (zh, zl))
      where
        (a', zl) = bitCoerce $ (xl `add` yl) + if c then 1 else 0
        (c', zh) = bitCoerce $ (xh `add` yh) + if a' then 1 else 0

    subC c = (a', c', byte (zh, zl))
      where
        (a', zl) = bitCoerce $ (xl `sub` yl) - if c then 1 else 0
        (c', zh) = bitCoerce $ (xh `sub` yh) - if a' then 1 else 0

    bcd = (a', c', z)
      where
        (a', w) = bitCoerce $ add y $ if yl > 9 || a then (0x06 :: Value) else 0
        (wh, _) = nybbles w
        (c', z) = bitCoerce $ add w $ if wh > 9 || c then (0x60 :: Value) else 0

shiftRotateALU :: Either ShiftRotate ShiftRotate -> (Bool, Value) -> (Bool, Value)
shiftRotateALU fun (c, x) = case fun of
    Left sr -> (b7, bitCoerce (b654321, b0, case sr of Shift -> c; Rotate -> b7))
    Right sr -> (b0, bitCoerce (case sr of Shift -> c; Rotate -> b0, b7, b654321))
  where
    (b7, b654321, b0) = bitCoerce x :: (Bool, Unsigned 6, Bool)
