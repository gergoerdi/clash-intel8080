{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.Intel8080.CPU where

import Clash.Prelude hiding (lift)

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Barbies

import Hardware.Intel8080
import Hardware.Intel8080.Decode
import Hardware.Intel8080.Microcode
import Hardware.Intel8080.MicroCPU

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Lens hiding (Index)
import Data.Maybe (fromMaybe, isJust)
import Data.Wedge

import Barbies
import Barbies.Bare
import Data.Barbie.TH
import qualified Language.Haskell.TH.Syntax as TH
import Debug.Trace

data Phase
    = Init
    | Halted
    | Fetching Bool
    | Executing Bool Value (Index MicroLen)
    deriving (Show, Generic, NFDataX)

declareBareB [d|
  data CPUIn = CPUIn
    { dataIn :: Maybe Value
    , interruptRequest :: Bool
    } |]

data CPUState = CPUState
    { _phase :: Phase
    , _interrupted :: Bool
    , _addrLatch :: Maybe (Either Port Addr)
    , _dataLatch :: Maybe Value
    , _microState :: MicroState
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''CPUState

initState :: Addr -> CPUState
initState pc0 = CPUState
    { _phase = Init
    , _interrupted = False
    , _addrLatch = Nothing
    , _dataLatch = Nothing
    , _microState = mkMicroState pc0
    }

declareBareB [d|
  data CPUOut = CPUOut
      { _addrOut :: Maybe (Either Port Addr)
      , _dataOut :: Maybe Value
      , _interruptAck :: Bool
      , _halted :: Bool
      } |]
makeLenses ''CPUOut

defaultOut :: CPUState -> Pure CPUOut
defaultOut CPUState{_microState = MicroState{..}, ..} = CPUOut{..}
  where
    _addrOut = _addrLatch
    _dataOut = _dataLatch
    _interruptAck = False
    _halted = case _phase of
        Halted -> True
        _ -> False

type CPU = MaybeT (CPUM CPUState CPUOut)

traceState :: (Show a) => CPU a -> CPU a
traceState act = do
    s <- zoom microState $ gets debugState
    x <- act
    trace (unlines [s, show x]) $ return x

latchInterrupt :: Pure CPUIn -> CPU Bool
latchInterrupt CPUIn{..} = do
    allowed <- use (microState.allowInterrupts)
    when (interruptRequest && allowed) $ interrupted .= True
    use interrupted

acceptInterrupt :: CPU ()
acceptInterrupt = do
    -- trace (show ("Interrupt accepted", pc)) $ return ()
    microState.allowInterrupts .= False
    addrLatch .= Nothing
    interrupted .= False
    interruptAck .:= True

readByte :: Pure CPUIn -> CPU Value
readByte CPUIn{..} = do
    pending <- isJust <$> use addrLatch
    if pending then maybe retry consume dataIn else return $ fromJustX dataIn
  where
    retry = mzero
    consume x = do
        addrLatch .= Nothing
        dataLatch .= Nothing
        return x

cpu :: Pure CPUIn -> CPUM CPUState CPUOut ()
cpu inp@CPUIn{..} = void . runMaybeT $ do
    interrupted <- latchInterrupt inp
    dataRead <- readByte inp

    use phase >>= \case
        Init -> do
            fetchNext
        Fetching False | interrupted -> do
            acceptInterrupt
            phase .= Fetching True
        Fetching interrupting -> do
            let instr = dataRead
            unless interrupting $ microState.pc += 1
            let setup = setupFor instr
            load <- addressing (wedgeRight setup)
            phase .= Executing load instr 0
        Executing load instr i -> do
            when load $ microState.valueBuf .= dataRead
            exec instr i
        Halted -> when interrupted $ do
            acceptInterrupt
            phase .= Fetching True

intel8080 :: (HiddenClockResetEnable dom) => Signals dom CPUIn -> Signals dom CPUOut
intel8080 = intel8080From 0x0000

intel8080From :: (HiddenClockResetEnable dom) => Addr -> Signals dom CPUIn -> Signals dom CPUOut
intel8080From startAddr = mealyCPU (initState startAddr) defaultOut cpu

fetchNext :: CPU ()
fetchNext = do
    addr <- Right <$> use (microState.pc)
    addrLatch .= Just addr
    phase .= Fetching False

exec :: Value -> Index MicroLen -> CPU ()
exec instr i = do
    let (uop, teardown) = microcodeFor instr !! i
    -- traceShow (i, uop, teardown) $ return ()
    runExceptT (zoom microState $ uexec uop) >>= \case
        Left GotoNext -> do
            fetchNext
        Left GotoHalt -> do
            phase .= Halted
        Right () -> do
            load <- addressing teardown
            maybe fetchNext (assign phase . Executing load instr) $ succIdx i

addressing :: Wedge OutAddr InAddr -> CPU Bool
addressing Nowhere = return False
addressing (Here write) = do
    doWrite =<< zoom microState (outAddr write)
    return False
addressing (There read) = do
    doRead =<< zoom microState (inAddr read)
    return True

doWrite :: Either Port Addr -> CPU ()
doWrite target = do
    addrLatch .= Just target
    value <- use $ microState.valueBuf
    dataLatch .= Just value

doRead :: Either Port Addr -> CPU ()
doRead addr = addrLatch .= Just addr

microcodeFor :: Value -> MicroOps
microcodeFor = asyncRom $(TH.lift $ map (snd . microcode . decodeInstr . bitCoerce) $ indicesI @256)

setupFor :: Value -> Setup
setupFor = asyncRom $(TH.lift $ map (fst . microcode . decodeInstr . bitCoerce) $ indicesI @256)
