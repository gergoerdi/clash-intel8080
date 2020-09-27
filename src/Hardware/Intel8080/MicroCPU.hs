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
    nextInstr :: m ()
    allowInterrupts :: Bool -> m ()

-- https://stackoverflow.com/a/36525016/477476
pairL :: Lens' s a -> Lens' s b -> Lens' s (a, b)
pairL l1 l2 = lens (view l1 &&& view l2) (\s (x,y) -> set l1 x . set l2 y $ s)

bitL :: (BitPack a, Enum i) => i -> Lens' a Bit
bitL i = lens (!i) (flip $ replaceBit i)

flag :: (MicroState s) => Flag -> Lens' s Bool
flag fl = reg RFlags . bitL fl . iso bitToBool boolToBit

evalCond :: (MicroM s m) => Cond -> m Bool
evalCond (Cond f target) = uses (flag f) (== target)

{-# INLINE uexec #-}
uexec :: (MicroM s m) => MicroInstr -> m ()
uexec (Get r) = assign valueBuf =<< use (reg r)
uexec (Set r) = assign (reg r) =<< use valueBuf
uexec FromPC = assign valueBuf =<< twistFrom pc
uexec FromAddrBuf = assign valueBuf =<< twistFrom addrBuf
uexec ToAddrBuf = twistTo addrBuf =<< use valueBuf
uexec (Get2 rp) = assign addrBuf =<< use (regPair rp)
uexec (Swap2 rp) = swap addrBuf (regPair rp)
uexec Jump = assign pc =<< use addrBuf
uexec (When cond) = do
    passed <- maybe (pure False) evalCond cond
    unless passed nextInstr
uexec (Compute arg fun updateC updateAC) = do
    c <- use (flag FC)
    x <- case arg of
        RegA -> use (reg RA)
        Const01 -> pure 0x01
        ConstFF -> pure 0xff
    y <- use valueBuf
    let (ac', c', result) = binALU fun c x y
    when (updateAC == SetAC) $ flag FAC .= ac'
    when (updateC == SetC) $ flag FC .= c'
    valueBuf .= result
uexec (ComputeSR sr) = do
    c <- use (flag FC)
    x <- use $ reg RA
    let (c', result) = shiftRotateALU sr c x
    flag FC .= c'
    valueBuf .= result
uexec (Compute2 fun2 updateC) = do
    arg <- case fun2 of
        Inc2 -> return 0x0001
        Dec2 -> return 0xffff
        AddHL -> use (regPair RHL)
    x <- use addrBuf
    let (c', x') = bitCoerce $ x `add` arg
    addrBuf .= x'
    when (updateC == SetC) $ flag FC .= c'
uexec (Compute0 fl fun0) = do
    flag fl %= case fun0 of
        ConstTrue0 -> const True
        Complement0 -> complement
uexec (Rst rst) = pc .= fromIntegral rst `shiftL` 3
uexec (SetInt b) = allowInterrupts b
uexec UpdateFlags = do
    x <- use valueBuf
    flag FZ .= (x == 0)
    flag FS .= x `testBit` 7
    flag FP .= even (popCount x)
uexec FixupBCD = do
    c <- use (flag FC)
    ac <- use (flag FAC)

    x <- use valueBuf
    (ac, x) <- return $
        let (_, x0) = bitCoerce x :: (Unsigned 4, Unsigned 4)
        in if x0 > 9 || ac then bitCoerce $ x `add` (0x06 :: Value) else (False, x)

    (c, x) <- return $
        let (x1, _) = bitCoerce x :: (Unsigned 4, Unsigned 4)
        in if x1 > 9 || c then bitCoerce $ x `add` (0x60 :: Value) else (False, x)

    flag FC .= c
    flag FAC .= ac
    valueBuf .= x

twist :: Addr -> (Value, Addr)
twist x = (hi, lohi)
  where
    (hi, lo) = bitCoerce x :: (Value, Value)
    lohi = bitCoerce (lo, hi)

twistFrom :: (MonadState s m) => Lens' s Addr -> m Value
twistFrom l = do
    (v, addr') <- twist <$> use l
    l .= addr'
    return v

twistTo :: (MonadState s m) => Lens' s Addr -> Value -> m ()
twistTo l x = do
    (y, _) <- twist <$> use l
    l .= bitCoerce (x, y)

swap :: (MonadState s m) => Lens' s a -> Lens' s a -> m ()
swap lx ly = do
    x <- use lx
    y <- use ly
    lx .= y
    ly .= x

inAddr :: (MicroState s, MonadState s m) => InAddr -> m (Either Port Addr)
inAddr FromPtr = Right <$> use addrBuf
inAddr FromPort = do
    (port, _) <- twist <$> use addrBuf
    return $ Left port
inAddr IncrPC = Right <$> (use pc <* (pc += 1))
inAddr IncrSP = Right <$> (use sp <* (sp += 1))

outAddr :: (MicroState s, MonadState s m) => OutAddr -> m (Either Port Addr)
outAddr ToPtr = Right <$> use addrBuf
outAddr ToPort = do
    (port, _) <- twist <$> use addrBuf
    return $ Left port
outAddr DecrSP = Right <$> ((sp -= 1) *> use sp)
