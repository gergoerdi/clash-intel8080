module Hardware.Intel8080.ALU (alu) where

import Prelude ()
import Data.Word
import Clash.Prelude
import Hardware.Intel8080

alu :: ALU -> Bool -> Value -> Value -> (Bool, Bool, Value)
alu fun c x y = case fun of
    ADD c0 -> addC x y (c0 && c)
    SUB c0 -> subC x y (c0 && c)
    AND -> (testBit (x .|. y) 4, False, x .&. y)
    OR -> (False, False, x .|. y)
    XOR -> (False, False, x `xor` y)
    SRRight sr -> (False, b0, bitCoerce (ifRotate sr b0 c, b7, b654321))
    SRLeft sr -> (False, b7, bitCoerce (b654321, b0, ifRotate sr b7 c))
  where
    ifRotate Rotate b c = b
    ifRotate Shift b c = c

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

    (b7, b654321, b0) = bitCoerce x :: (Bool, Unsigned 6, Bool)
