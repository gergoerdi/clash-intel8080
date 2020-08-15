{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hardware.Intel8080.CPU where

import Clash.Prelude hiding (lift)

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Barbies

import Hardware.Intel8080
import Hardware.Intel8080.Decode
import Hardware.Intel8080.Microcode
import qualified Hardware.Intel8080.MicroCPU as MCPU

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Lens hiding (Index)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)

import Barbies
import Barbies.Bare
import Data.Barbie.TH

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
    { dataIn :: Maybe Value
    , interruptRequest :: Bool
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
      { _addrOut :: Addr
      , _dataOut :: Maybe Value
      , _portSelect :: Bool
      , _interruptAck :: Bool
      } |]
makeLenses ''CPUOut

defaultOut :: CPUState -> Pure CPUOut
defaultOut CPUState{..} = CPUOut{..}
  where
    _addrOut = _addrBuf
    _dataOut = Nothing
    _portSelect = False
    _interruptAck = False

type M = MaybeT (CPUM CPUState CPUOut)

pretty :: M String
pretty = do
    pc <- use pc
    sp <- use sp
    ~[bc, de, hl, af] <- mapM (use . MCPU.regPair . uncurry Regs) [(rB, rC), (rD, rE), (rH, rL), (rA, rFlags)]
    return $ unlines
      [ printf "IR:         PC: 0x%04x  SP: 0x%04x" pc sp
      , printf "BC: 0x%04x  DE: 0x%04x  HL: 0x%04x  AF: 0x%04x" bc de hl af
      ]

traceState :: (Show a) => M a -> M a
traceState act = do
    s <- pretty
    x <- act
    trace (unlines [s, show x]) $ return x

instance MCPU.MicroState CPUState where
    reg r = registers . lens (!! r) (\s v -> replace r v s)
    pc = pc
    sp = sp
    valueBuf = valueBuf
    addrBuf = addrBuf

instance MCPU.MicroM CPUState (ReaderT (Maybe Value) M) where
    write = assignOut dataOut . Just
    readByte = maybe mzero return =<< ask
    nextInstr = lift nextInstr >> mzero
    allowInterrupts = assign allowInterrupts

latchInterrupt :: Pure CPUIn -> M Bool
latchInterrupt CPUIn{..} = do
    allowed <- use allowInterrupts
    when (interruptRequest && allowed) $ interrupted .= True
    use interrupted

acceptInterrupt :: M ()
acceptInterrupt = do
    -- trace (show ("Interrupt accepted", pc)) $ return ()
    allowInterrupts .= False
    interrupted .= False
    interruptAck .:= True

readByte :: Pure CPUIn -> M Value
readByte CPUIn{..} = maybe mzero return dataIn

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
            runReaderT (MCPU.uexec uop) dataIn
            traverse_ addressing teardown
            maybe nextInstr (assign phase . Executing) $ succIdx i

nextInstr :: M ()
nextInstr = do
    assignOut addrOut =<< use pc
    phase .= Fetching False

addressing :: Addressing -> M ()
addressing Port = do
    (port, _) <- MCPU.twist <$> use addrBuf
    tellPort port
addressing Indirect = assignOut addrOut =<< use addrBuf
addressing IncrPC = assignOut addrOut =<< use pc <* (pc += 1)
addressing IncrSP = assignOut addrOut =<< use sp  <* (sp += 1)
addressing DecrSP = assignOut addrOut =<< (sp -= 1) *> use sp

tellPort :: Value -> M ()
tellPort port = do
    portSelect .:= True
    addrOut .:= bitCoerce (port, port)
