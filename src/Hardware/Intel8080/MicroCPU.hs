{-# LANGUAGE RecordWildCards, LambdaCase, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Hardware.Intel8080.MicroCPU where

import Prelude ()
import Clash.Prelude hiding (lift)

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Barbies

import Hardware.Intel8080
import Hardware.Intel8080.ALU
import Hardware.Intel8080.Microcode

import Control.Lens hiding (Index)
import Control.Monad.State
import Control.Arrow ((&&&))

class MicroState s where
    reg :: Reg -> Lens' s Value
    regPair :: RegPair -> Lens' s Addr

    regPair (Regs r1 r2) = pairL (reg r1) (reg r2) . iso bitCoerce bitCoerce
    regPair SP = sp

    pc :: Lens' s Addr
    sp :: Lens' s Addr

    valueBuf :: Lens' s Value
    addrBuf :: Lens' s Addr

class (MicroState s, MonadState s m) => MicroM s m | m -> s where
    writeOut :: Value -> m ()
    readIn :: m Value
    nextInstr :: m ()
    allowInterrupts :: Bool -> m ()

-- https://stackoverflow.com/a/36525016/477476
pairL :: Lens' s a -> Lens' s b -> Lens' s (a, b)
pairL l1 l2 = lens (view l1 &&& view l2) (\s (x,y) -> set l1 x . set l2 y $ s)

bitL :: (BitPack a, Enum i) => i -> Lens' a Bit
bitL i = lens (!i) (flip $ replaceBit i)

flag :: (MicroState s) => Flag -> Lens' s Bool
flag fl = reg rFlags . bitL fl . iso bitToBool boolToBit

evalCond :: (MicroM s m) => Cond -> m Bool
evalCond (Cond f target) = uses (flag f) (== target)

{-# INLINE uexec #-}
uexec :: (MicroM s m) => Effect -> m ()
uexec (Get r) = assign valueBuf =<< use (reg r)
uexec (Set r) = assign (reg r) =<< use valueBuf
uexec (Get2 rp) = assign addrBuf =<< use (regPair rp)
uexec (Swap2 rp) = do
    tmp <- use addrBuf
    assign addrBuf =<< use (regPair rp)
    regPair rp .= tmp
uexec Jump = assign pc =<< use addrBuf
uexec (ReadMem target) = do
    x <- readIn
    case target of
        ValueBuf -> valueBuf .= x
        AddrBuf -> do
            (y, _) <- twist <$> use addrBuf
            addrBuf .= bitCoerce (x, y)
        PC -> do
            (y, _) <- twist <$> use pc
            assign pc $ bitCoerce (x, y)
uexec (WriteMem target) = writeOut =<< case target of
    ValueBuf -> use valueBuf
    AddrBuf -> do
        (v, addr') <- twist <$> use addrBuf
        addrBuf .= addr'
        return v
    PC -> do
        (v, pc') <- twist <$> use pc
        pc .= pc'
        return v
uexec (When cond) = do
    passed <- maybe (pure False) evalCond cond
    unless passed nextInstr
uexec (Compute arg fun updateC updateA) = do
    c <- use (flag fC)
    x <- case arg of
        RegA -> use (reg rA)
        Const01 -> pure 0x01
        ConstFF -> pure 0xff
    y <- use valueBuf
    let (a', c', result) = alu fun c x y
    when (updateC == SetC) $ flag fC .= c'
    when (updateA == SetA) $ flag fA .= a'
    valueBuf .= result
uexec (Compute2 fun2 updateC) = do
    arg <- case fun2 of
        Inc2 -> return 0x0001
        Dec2 -> return 0xffff
        AddHL -> use (regPair rHL)
    x <- use addrBuf
    let (c', x') = bitCoerce $ x `add` arg
    addrBuf .= x'
    when (updateC == SetC) $ flag fC .= c'
uexec (Compute0 fl fun0) = do
    flag fl %= case fun0 of
        ConstTrue0 -> const True
        Complement0 -> complement
uexec (Rst rst) = pc .= fromIntegral rst `shiftL` 3
uexec (SetInt b) = allowInterrupts b
uexec UpdateFlags = do
    x <- use valueBuf
    flag fZ .= (x == 0)
    flag fS .= x `testBit` 7
    flag fP .= even (popCount x)
uexec FixupBCD = do
    a <- use (flag fA)
    c <- use (flag fC)

    x <- use valueBuf
    (a, x) <- return $
        let (_, x0) = bitCoerce x :: (Unsigned 4, Unsigned 4)
        in if x0 > 9 || a then bitCoerce $ x `add` (0x06 :: Value) else (False, x)

    (c, x) <- return $
        let (x1, _) = bitCoerce x :: (Unsigned 4, Unsigned 4)
        in if x1 > 9 || c then bitCoerce $ x `add` (0x60 :: Value) else (False, x)

    flag fA .= a
    flag fC .= c
    valueBuf .= x

twist :: Addr -> (Value, Addr)
twist x = (hi, lohi)
  where
    (hi, lo) = bitCoerce x :: (Value, Value)
    lohi = bitCoerce (lo, hi)

swap :: (MonadState s m) => Lens' s a -> Lens' s a -> m ()
swap lx ly = do
    x <- use lx
    y <- use ly
    lx .= y
    ly .= x
