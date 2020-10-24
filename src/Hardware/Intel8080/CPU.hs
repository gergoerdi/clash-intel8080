{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Lens hiding (Index)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Wedge

import Barbies
import Barbies.Bare
import Data.Barbie.TH
import qualified Language.Haskell.TH.Syntax as TH

import Debug.Trace
import Text.Printf

data Phase
    = Init
    | Halted
    | Fetching Bool
    | Executing Value Bool (Index MicroLen)
    deriving (Show, Generic, NFDataX)

declareBareB [d|
  data CPUIn = CPUIn
    { dataIn :: Maybe Value
    , interruptRequest :: Bool
    } |]

data CPUState = CPUState
    { _phase :: Phase
    , _interrupted :: Bool
    , _addrLatch :: Maybe Addr
    , _microState :: MicroState
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''CPUState

initState :: Addr -> CPUState
initState pc0 = CPUState
    { _phase = Init
    , _interrupted = False
    , _addrLatch = Nothing
    , _microState = mkMicroState pc0
    }

declareBareB [d|
  data CPUOut = CPUOut
      { _addrOut :: Either Port Addr
      , _dataOut :: Maybe Value
      , _interruptAck :: Bool
      , _halted :: Bool
      } |]
makeLenses ''CPUOut

defaultOut :: CPUState -> Pure CPUOut
defaultOut CPUState{_microState = MicroState{..}, ..} = CPUOut{..}
  where
    _addrOut = Right $ fromMaybe _addrBuf _addrLatch
    _dataOut = Nothing
    _interruptAck = False
    _halted = case _phase of
        Halted -> True
        _ -> False

type M = MaybeT (CPUM CPUState CPUOut)

pretty :: M String
pretty = zoom microState $ do
    pc <- use pc
    sp <- use sp
    valueBuf <- use valueBuf
    addrBuf <- use addrBuf
    ~[bc, de, hl, af] <- mapM (use . regPair . uncurry Regs) [(RB, RC), (RD, RE), (RH, RL), (RA, RFlags)]
    return $ unlines
      [ printf "IR:         PC: 0x%04x  SP: 0x%04x  U1:   0x%02x  U2: 0x%04x" pc sp valueBuf addrBuf
      , printf "BC: 0x%04x  DE: 0x%04x  HL: 0x%04x  AF: 0x%04x" bc de hl af
      ]

traceState :: (Show a) => M a -> M a
traceState act = do
    s <- pretty
    x <- act
    trace (unlines [s, show x]) $ return x

latchInterrupt :: Pure CPUIn -> M Bool
latchInterrupt CPUIn{..} = do
    allowed <- use (microState.allowInterrupts)
    when (interruptRequest && allowed) $ interrupted .= True
    use interrupted

acceptInterrupt :: M ()
acceptInterrupt = do
    -- trace (show ("Interrupt accepted", pc)) $ return ()
    microState.allowInterrupts .= False
    interrupted .= False
    interruptAck .:= True

readByte :: Pure CPUIn -> M Value
readByte CPUIn{..} = readFrom dataIn

readFrom :: Maybe Value -> M Value
readFrom = maybe retry ack
  where
    retry = mzero
    ack x = do
        addrLatch .= Nothing
        return x

fetch :: Pure CPUIn -> M Value
fetch inp = do
    x <- readByte inp
    microState.pc += 1
    return x

cpuMachine :: Pure CPUIn -> State CPUState (Pure CPUOut)
cpuMachine inp = do
    s0 <- get
    (x, out) <- runWriterT . runMaybeT $ cpu inp
    case x of
        Nothing -> do
            put s0
        Just () -> return ()
    gets $ \s -> update (defaultOut s) out

cpu :: Pure CPUIn -> M ()
cpu inp@CPUIn{..} = do
    interrupted <- latchInterrupt inp

    use phase >>= \case
        Halted -> mzero
        Init -> do
            fetchNext
        Fetching False | interrupted -> do
            acceptInterrupt
            phase .= Fetching True
        Fetching interrupting -> do
            instr <- {- traceState $ -} if interrupting then readByte inp else fetch inp
            let (setup, _) = microcodeFor instr
            load <- addressing (wedgeRight setup)
            phase .= Executing instr load 0
        Executing instr load i -> do
            when load $ assign (microState.valueBuf) =<< readFrom dataIn

            let (uop, teardown) = snd (microcodeFor instr) !! i
            -- traceShow (i, uop, teardown) $ return ()
            x <- runExceptT $ zoom microState $ uexec uop
            case x of
                Left GotoNext -> do
                    fetchNext
                Left GotoHalt -> do
                    phase .= Halted
                Right () -> do
                    load' <- addressing teardown
                    maybe fetchNext (assign phase . Executing instr load') $ succIdx i

fetchNext :: M ()
fetchNext = do
    latchAddr =<< use (microState.pc)
    phase .= Fetching False

addressing :: Wedge OutAddr InAddr -> M Bool
addressing Nowhere = return False
addressing (Here write) = do
    doWrite =<< zoom microState (outAddr write)
    return False
addressing (There read) = do
    doRead =<< zoom microState (inAddr read)
    return True

doWrite :: Either Port Addr -> M ()
doWrite target = do
    addrOut .:= target
    value <- use (microState.valueBuf)
    dataOut .:= Just value

doRead :: Either Port Addr -> M ()
doRead target = either tellPort latchAddr target

tellPort :: Value -> M ()
tellPort port = do
    addrOut .:= Left port

latchAddr :: Addr -> M ()
latchAddr addr = do
    addrLatch .= Just addr
    addrOut .:= Right addr

microcodeFor :: Value -> Microcode
microcodeFor = asyncRom $(TH.lift $ map (microcode . decodeInstr . bitCoerce) $ indicesI @256)
