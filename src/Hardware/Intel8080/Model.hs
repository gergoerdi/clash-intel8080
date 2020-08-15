{-# LANGUAGE RecordWildCards #-}
module Hardware.Intel8080.Model where

import Hardware.Intel8080
import Hardware.Intel8080.ISA
import Hardware.Intel8080.ALU
import Hardware.Intel8080.Decode
import Hardware.Intel8080.Microcode

import Prelude ()
import Clash.Prelude hiding (lift)
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Foldable
import Control.Monad.Extra (whenM, unlessM)
import Control.Lens hiding (index)

import Debug.Trace
import Text.Printf

data S = MkS
    { _pc :: Addr
    , _sp :: Addr
    , _allowInterrupts :: Bool
    , _registers :: Vec 8 Value
    , _ureg1 :: Value
    , _ureg2 :: Addr
    , _targetPort :: Bool
    , _addr :: Addr
    , _write :: Maybe Value
    }
    deriving (Show)
makeLenses ''S

mkS :: S
mkS = MkS{..}
  where
    _pc = 0
    _sp = 0
    _allowInterrupts = False
    _registers = replace 1 0x02 $ pure 0x00
    _ureg1 = 0
    _ureg2 = 0
    _targetPort = False
    _addr = 0
    _write = Nothing

data R = MkR
    { readMem :: Addr -> IO Value
    , writeMem :: Addr -> Value -> IO ()
    , inPort :: Port -> IO Value
    , outPort :: Port -> Value -> IO Value
    }

type CPU = MaybeT (RWST R () S IO)

instance Intel8080 CPU where
    getReg r = uses registers (!! r)
    setReg r v = registers %= replace r v

    getSP = use sp
    setSP addr = sp .= addr

dumpState :: CPU ()
dumpState = do
    pc <- use pc
    sp <- use sp
    [bc, de, hl, af] <- mapM (getRegPair . uncurry Regs) [(rB, rC), (rD, rE), (rH, rL), (rA, rFlags)]
    liftIO $ do
        printf "IR:         PC: 0x%04x  SP: 0x%04x\n" pc sp
        printf "BC: 0x%04x  DE: 0x%04x  HL: 0x%04x  AF: 0x%04x\n" bc de hl af

peekByte :: Addr -> CPU Value
peekByte addr = do
    readMem <- asks readMem
    liftIO $ readMem addr

readByte :: CPU Value
readByte = do
    isPort <- use targetPort
    addr <- use addr
    let port = fromIntegral addr
    if isPort then readPort port else peekByte addr

writeByte :: Value -> CPU ()
writeByte x = do
    isPort <- use targetPort
    addr <- use addr
    let port = fromIntegral addr
    if isPort then writePort port x else pokeByte addr x

pokeByte :: Addr -> Value -> CPU ()
pokeByte addr x = do
    writeMem <- asks writeMem
    liftIO $ writeMem addr x

fetchByte :: CPU Value
fetchByte = do
    pc <- use pc <* (pc += 1)
    peekByte pc

writePort :: Port -> Value -> CPU ()
writePort port value = do
    write <- asks outPort
    liftIO $ void $ write port value

readPort :: Port -> CPU Value
readPort port = do
    read <- asks inPort
    liftIO $ read port

step :: CPU ()
step = do
    instr <- decodeInstr <$> fetchByte
    exec instr

interrupt :: Instr -> CPU ()
interrupt instr = whenM (use allowInterrupts) $ do
    allowInterrupts .= False
    exec instr

exec :: Instr -> CPU ()
exec instr = do
    uinit
    let (setup, uops) = microcode instr
    traverse_ addressing setup
    -- liftIO $ print (instr, uops)
    mapM_ ustep uops

uinit :: CPU ()
uinit = do
    targetPort .= False

addressing :: Addressing -> CPU ()
addressing Indirect = do
    assign addr =<< use ureg2
addressing Port = do
    (port, _) <- twist <$> use ureg2
    tellPort port
addressing IncrPC = assign addr =<< use pc <* (pc += 1)
addressing IncrSP = assign addr =<< use sp <* (sp += 1)
addressing DecrSP = assign addr =<< (sp -= 1) *> use sp

tellPort :: Value -> CPU ()
tellPort port = do
    targetPort .= True
    addr .= bitCoerce (port, port)

twist :: Addr -> (Value, Addr)
twist x = (hi, lohi)
  where
    (hi, lo) = bitCoerce x :: (Value, Value)
    lohi = bitCoerce (lo, hi)

ustep :: MicroOp -> CPU ()
ustep (effect, post) = do
    write .= Nothing
    uexec effect
    traverse_ addressing post
    traverse_ writeByte =<< use write

uexec :: Effect -> CPU ()
uexec (Get r) = assign ureg1 =<< getReg r
uexec (Set r) = setReg r =<< use ureg1
uexec (Get2 rp) = assign ureg2 =<< getRegPair rp
uexec (Swap2 rp) = do
    tmp <- use ureg2
    assign ureg2 =<< getRegPair rp
    setRegPair rp tmp
uexec Jump = assign pc =<< use ureg2
uexec (ReadMem target) = do
    x <- readByte
    case target of
        ValueBuf -> ureg1 .= x
        AddrBuf -> do
            (y, _) <- twist <$> use ureg2
            ureg2 .= bitCoerce (x, y)
        PC -> do
            (y, _) <- twist <$> use pc
            pc .= bitCoerce (x, y)
uexec (WriteMem target) = do
    assign write . Just =<< case target of
        ValueBuf -> use ureg1
        AddrBuf -> do
            (v, addr') <- twist <$> use ureg2
            ureg2 .= addr'
            return v
        PC -> do
            (v, pc') <- twist <$> use pc
            pc .= pc'
            return v
uexec (When cond) = do
    passed <- maybe (pure False) evalCond cond
    guard passed
uexec (Compute arg fun updateC updateA) = do
    c <- getFlag fC
    x <- case arg of
        RegA -> getReg rA
        Const01 -> pure 0x01
        ConstFF -> pure 0xff
    y <- use ureg1
    let (a', c', result) = alu fun c x y
    when (updateC == SetC) $ setFlag fC c'
    when (updateA == SetA) $ setFlag fA a'
    ureg1 .= result
uexec (Compute2 fun2 updateC) = do
    arg <- case fun2 of
        Inc2 -> return 0x0001
        Dec2 -> return 0xffff
        AddHL -> getRegPair rHL
    x <- use ureg2
    let (c', x') = bitCoerce $ x `add` arg
    ureg2 .= x'
    when (updateC == SetC) $ setFlag fC c'
uexec (Compute0 flag fun0) = do
    f <- getFlag flag
    setFlag flag $ case fun0 of
        ConstTrue0 -> True
        Complement0 -> complement f
uexec (Rst rst) = pc .= fromIntegral rst `shiftL` 3
uexec (SetInt b) = allowInterrupts .= b
uexec UpdateFlags = do
    x <- use ureg1
    setFlag fZ (x == 0)
    setFlag fS (x `testBit` 7)
    setFlag fP (even $ popCount x)
uexec FixupBCD = do
    a <- getFlag fA
    c <- getFlag fC

    x <- use ureg1
    (a, x) <- return $
        let (_, x0) = bitCoerce x :: (Unsigned 4, Unsigned 4)
        in if x0 > 9 || a then bitCoerce $ x `add` (0x06 :: Value) else (False, x)

    (c, x) <- return $
        let (x1, _) = bitCoerce x :: (Unsigned 4, Unsigned 4)
        in if x1 > 9 || c then bitCoerce $ x `add` (0x60 :: Value) else (False, x)

    setFlag fA a
    setFlag fC c
    ureg1 .= x
