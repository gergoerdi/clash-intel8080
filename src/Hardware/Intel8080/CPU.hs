{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Hardware.Intel8080.CPU where

import Prelude ()
import Clash.Prelude hiding (lift)

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Barbies

import Hardware.Intel8080
import Hardware.Intel8080.ISA
import Hardware.Intel8080.Decode
import Hardware.Intel8080.ALU
import Hardware.Intel8080.Microcode
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Extra (whenM)
import Control.Lens hiding (Index)

import Barbies
import Barbies.Bare
import Data.Barbie.TH

import Control.Monad.State
import Data.Word
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)

import Debug.Trace
import Text.Printf

data Phase
    = Init
    | Halted
    | Fetching Bool
    | Executing (Index MicroLen)
    deriving (Show, Generic, NFDataX)

declareBareB [d|
  data CPUIn = CPUIn
    { cpuInMem :: Maybe Value
    , cpuInIRQ :: Bool
    } |]

data CPUState = CPUState
    { _phase :: Phase
    , _pc, _sp :: Addr
    , _registers :: Vec 8 Value
    , _allowInterrupts :: Bool
    , _interrupted :: Bool
    , _instrBuf :: Instr
    , _valueBuf :: Value
    , _addrBuf :: Addr
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''CPUState

initState :: CPUState
initState = CPUState
    { _phase = Init
    , _pc = 0x0000
    , _sp = 0x0000
    , _registers = replace 1 0x02 $ pure 0x00
    , _allowInterrupts = False
    , _interrupted = False
    , _instrBuf = NOP
    , _valueBuf = 0x00
    , _addrBuf = 0x0000
    }

declareBareB [d|
  data CPUOut = CPUOut
      { _cpuOutMemAddr :: Addr
      , _cpuOutMemWrite :: Maybe Value
      , _cpuOutPortSelect :: Bool
      , _cpuOutIRQAck :: Bool
      } |]
makeLenses ''CPUOut

defaultOut :: CPUState -> Pure CPUOut
defaultOut CPUState{..} = CPUOut{..}
  where
    _cpuOutMemAddr = _addrBuf
    _cpuOutMemWrite = Nothing
    _cpuOutPortSelect = False
    _cpuOutIRQAck = False

type M = MaybeT (CPUM CPUState CPUOut)

pretty :: M String
pretty = do
    pc <- use pc
    sp <- use sp
    ~[bc, de, hl, af] <- mapM (getRegPair . uncurry Regs) [(rB, rC), (rD, rE), (rH, rL), (rA, rFlags)]
    return $ unlines
      [ printf "IR:         PC: 0x%04x  SP: 0x%04x" pc sp
      , printf "BC: 0x%04x  DE: 0x%04x  HL: 0x%04x  AF: 0x%04x" bc de hl af
      ]

traceState :: (Show a) => M a -> M a
traceState act = do
    s <- pretty
    x <- act
    trace (unlines [s, show x]) $ return x

instance Intel8080 M where
    getReg r = uses registers (!! r)
    {-# INLINE getReg #-}

    setReg r v = registers %= replace r v
    {-# INLINE setReg #-}

    getSP = use sp
    {-# INLINE getSP #-}

    setSP addr = sp .= addr
    {-# INLINE setSP #-}

latchInterrupt :: Pure CPUIn -> M Bool
latchInterrupt CPUIn{..} = do
    allowed <- use allowInterrupts
    when (cpuInIRQ && allowed) $ interrupted .= True
    use interrupted

acceptInterrupt :: M ()
acceptInterrupt = do
    -- trace (show ("Interrupt accepted", pc)) $ return ()
    allowInterrupts .= False
    interrupted .= False
    cpuOutIRQAck .:= True

readByte :: Pure CPUIn -> M Value
readByte CPUIn{..} = maybe mzero return cpuInMem

fetch :: Pure CPUIn -> M Value
fetch inp = do
    x <- readByte inp
    pc += 1
    return x

cpuMachine :: Pure CPUIn -> State CPUState (Pure CPUOut)
cpuMachine = runCPU defaultOut . void . runMaybeT . cpu

cpu :: Pure CPUIn -> M ()
cpu inp@CPUIn{..} = do
    interrupted <- latchInterrupt inp

    use phase >>= \case
        Halted -> mzero
        Init -> do
            assign addrBuf =<< use pc
            phase .= Fetching False
        Fetching False | interrupted -> do
            acceptInterrupt
            phase .= Fetching True
        Fetching interrupting -> do
            instr <- {- traceState $ -} decodeInstr <$> if interrupting then readByte inp else fetch inp
            instrBuf .= instr
            let (setup, _) = microcode instr
            traverse_ addressing setup
            phase .= Executing 0
        Executing i -> do
            instr <- use instrBuf
            let (uop, teardown) = snd (microcode instr) !! i
            -- traceShow (i, uop, teardown) $ return ()
            uexec inp uop
            traverse_ addressing teardown
            maybe nextInstr (assign phase . Executing) $ succIdx i

nextInstr :: M ()
nextInstr = do
    assignOut cpuOutMemAddr =<< use pc
    phase .= Fetching False

addressing :: Addressing -> M ()
addressing Indirect = do
    assignOut cpuOutMemAddr =<< use addrBuf
addressing Port = do
    (port, _) <- twist <$> use addrBuf
    tellPort port
addressing IncrPC = assignOut cpuOutMemAddr =<< use pc <* (pc += 1)
addressing IncrSP = assignOut cpuOutMemAddr =<< use sp  <* (sp += 1)
addressing DecrSP = assignOut cpuOutMemAddr =<< (sp -= 1) *> use sp

uexec :: Pure CPUIn -> Effect -> M ()
uexec inp (Get r) = assign valueBuf =<< getReg r
uexec inp (Set r) = setReg r =<< use valueBuf
uexec inp (Get2 rp) = assign addrBuf =<< getRegPair rp
uexec inp (Swap2 rp) = do
    tmp <- use addrBuf
    assign addrBuf =<< getRegPair rp
    setRegPair rp tmp
uexec inp Jump = assign pc =<< use addrBuf
uexec inp (ReadMem target) = do
    x <- readByte inp
    case target of
        ValueBuf -> valueBuf .= x
        AddrBuf -> do
            (y, _) <- twist <$> use addrBuf
            addrBuf .= bitCoerce (x, y)
        PC -> do
            (y, _) <- twist <$> use pc
            pc .= bitCoerce (x, y)
uexec inp (WriteMem target) = do
    assignOut cpuOutMemWrite . Just =<< case target of
        ValueBuf -> use valueBuf
        AddrBuf -> do
            (v, addr') <- twist <$> use addrBuf
            addrBuf .= addr'
            return v
        PC -> do
            (v, pc') <- twist <$> use pc
            pc .= pc'
            return v
uexec inp (When cond) = do
    passed <- maybe (pure False) evalCond cond
    unless passed $ nextInstr >> mzero
uexec inp (Compute arg fun updateC updateA) = do
    c <- getFlag fC
    x <- case arg of
        RegA -> getReg rA
        Const01 -> pure 0x01
        ConstFF -> pure 0xff
    y <- use valueBuf
    let (a', c', result) = alu fun c x y
    when (updateC == SetC) $ setFlag fC c'
    when (updateA == SetA) $ setFlag fA a'
    valueBuf .= result
uexec inp (Compute2 fun2 updateC) = do
    arg <- case fun2 of
        Inc2 -> return 0x0001
        Dec2 -> return 0xffff
        AddHL -> getRegPair rHL
    x <- use addrBuf
    let (c', x') = bitCoerce $ x `add` arg
    addrBuf .= x'
    when (updateC == SetC) $ setFlag fC c'
uexec inp (Compute0 flag fun0) = do
    f <- getFlag flag
    setFlag flag $ case fun0 of
        ConstTrue0 -> True
        Complement0 -> complement f
uexec inp (Rst rst) = pc .= fromIntegral rst `shiftL` 3
uexec inp (SetInt b) = allowInterrupts .= b
uexec inp UpdateFlags = do
    x <- use valueBuf
    setFlag fZ (x == 0)
    setFlag fS (x `testBit` 7)
    setFlag fP (even $ popCount x)
uexec inp FixupBCD = do
    a <- getFlag fA
    c <- getFlag fC

    x <- use valueBuf
    (a, x) <- return $
        let (_, x0) = bitCoerce x :: (Unsigned 4, Unsigned 4)
        in if x0 > 9 || a then bitCoerce $ x `add` (0x06 :: Value) else (False, x)

    (c, x) <- return $
        let (x1, _) = bitCoerce x :: (Unsigned 4, Unsigned 4)
        in if x1 > 9 || c then bitCoerce $ x `add` (0x60 :: Value) else (False, x)

    setFlag fA a
    setFlag fC c
    valueBuf .= x

twist :: Addr -> (Value, Addr)
twist x = (hi, lohi)
  where
    (hi, lo) = bitCoerce x :: (Value, Value)
    lohi = bitCoerce (lo, hi)

tellPort :: Value -> M ()
tellPort port = do
    cpuOutPortSelect .:= True
    cpuOutMemAddr .:= bitCoerce (port, port)
