{-# LANGUAGE RecordWildCards, RankNTypes #-}
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
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Text.Printf

data FlowControl
    = GotoNext
    | GotoHalt
    deriving (Show, Eq, Enum, Bounded, Generic, NFDataX, Lift)

data MicroState = MicroState
    { _pc, _sp :: Addr
    , _registers :: Vec 8 Value
    , _allowInterrupts :: Bool
    , _valueBuf :: Value
    , _addrBuf :: Addr
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''MicroState

mkMicroState :: Addr -> MicroState
mkMicroState pc0 = MicroState{..}
  where
    _pc = pc0
    _sp = 0
    _allowInterrupts = False
    _registers = repeat 0x00
    _valueBuf = 0
    _addrBuf = 0

debugState :: MicroState -> String
debugState s@MicroState{..} = unlines
    [ printf "Addr: 0x%04x  PC: 0x%04x  SP: 0x%04x  Val:   0x%02x" _addrBuf _pc _sp _valueBuf
    , printf "BC:   0x%04x  DE: 0x%04x  HL: 0x%04x  AF:  0x%04x" bc de hl af
    ]
  where
    [bc, de, hl, af] = fmap (\rr -> s ^. regPair rr) [RBC, RDE, RHL, RAF]

reg :: Reg -> Lens' MicroState Value
reg r = registers . lens (fixup . (!! r)) (\s v -> replace r v s)
  where
    fixup = case r of
        RFlags -> (`clearBit` 5) . (`clearBit` 3) . (`setBit` 1)
        _ -> id

regPair :: RegPair -> Lens' MicroState Addr
regPair (Regs r1 r2) = unsafePairL (reg r1) (reg r2) . iso bitCoerce bitCoerce
regPair SP = sp

-- https://stackoverflow.com/a/36525016/477476
unsafePairL :: Lens' s a -> Lens' s b -> Lens' s (a, b)
unsafePairL l1 l2 = lens (view l1 &&& view l2) (\s (x,y) -> set l1 x . set l2 y $ s)

bitL :: (BitPack a, Enum i) => i -> Lens' a Bit
bitL i = lens (! i) (flip $ replaceBit i)

flag :: Flag -> Lens' MicroState Bool
flag flg = reg RFlags . bitL flg . iso bitToBool boolToBit

evalCond :: (MonadState MicroState m) => Cond -> m Bool
evalCond (Cond flg target) = uses (flag flg) (== target)

{-# INLINE uexec #-}
uexec :: (MonadState MicroState m) => MicroInstr -> ExceptT FlowControl m ()
uexec Halt = throwError GotoHalt
uexec (FromReg r) = valueBuf <~ use (reg r)
uexec (ToReg r) = reg r <~ use valueBuf
uexec FromAddrBuf = valueBuf <~ twistFrom addrBuf
uexec FromPC = valueBuf <~ twistFrom pc
uexec ToAddrBuf = twistTo addrBuf =<< use valueBuf
uexec (FromReg2 rp) = addrBuf <~ use (regPair rp)
uexec (SwapReg2 rp) = swap addrBuf (regPair rp)
uexec Jump = pc <~ use addrBuf
uexec (When cond) = do
    passed <- evalCond cond
    unless passed $ throwError GotoNext
uexec (Compute arg fun updateZSP updateAC updateC) = do
    x <- case arg of
        RegA -> use (reg RA)
        AddrLo -> truncateB <$> use addrBuf
        Const01 -> pure 0x01
        ConstFF -> pure 0xff
    ac <- use (flag FAC)
    c <- use (flag FC)
    y <- use valueBuf
    let (ac', c', y') = binALU fun x (ac, c, y)
    when (updateZSP == SetZSP) $ do
        flag FZ .= (y' == 0)
        flag FS .= y' `testBit` 7
        flag FP .= even (popCount y')
    when (updateAC == SetAC) $ flag FAC .= ac'
    when (updateC == SetC) $ flag FC .= c'
    valueBuf .= y'
uexec (ComputeSR sr) = do
    c <- use (flag FC)
    x <- use $ reg RA
    let (c', x') = shiftRotateALU sr (c, x)
    flag FC .= c'
    valueBuf .= x'
uexec (Compute2 fun) = do
    addrBuf %= case fun of
        Inc -> (+ 1)
        Dec -> subtract 1
uexec (Compute0 flg fun) = do
    flag flg %= case fun of
        ConstTrue -> const True
        Complement -> complement
uexec (Rst rst) = pc .= extend rst `shiftL` 3
uexec (SetInt b) = allowInterrupts .= b

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

inAddr :: (MonadState MicroState m) => InAddr -> m (Either Port Addr)
inAddr FromPtr = Right <$> use addrBuf
inAddr FromPort = do
    (port, _) <- twist <$> use addrBuf
    return $ Left port
inAddr IncrPC = Right <$> (use pc <* (pc += 1))
inAddr IncrSP = Right <$> (use sp <* (sp += 1))

outAddr :: (MonadState MicroState m) => OutAddr -> m (Either Port Addr)
outAddr ToPtr = Right <$> use addrBuf
outAddr ToPort = do
    (port, _) <- twist <$> use addrBuf
    return $ Left port
outAddr DecrSP = Right <$> ((sp -= 1) *> use sp)
