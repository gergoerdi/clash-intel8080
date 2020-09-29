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
import qualified Hardware.Intel8080.MicroCPU as MCPU

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
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
    , _pc, _sp :: Addr
    , _registers :: Vec 8 Value
    , _allowInterrupts :: Bool
    , _interrupted :: Bool
    , _valueBuf :: Value
    , _addrBuf :: Addr
    , _addrLatch :: Maybe Addr
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
    , _valueBuf = 0x00
    , _addrBuf = 0x0000
    , _addrLatch = Nothing
    }

declareBareB [d|
  data CPUOut = CPUOut
      { _addrOut :: Either Port Addr
      , _dataOut :: Maybe Value
      , _interruptAck :: Bool
      } |]
makeLenses ''CPUOut

defaultOut :: CPUState -> Pure CPUOut
defaultOut CPUState{..} = CPUOut{..}
  where
    _addrOut = Right $ fromMaybe _addrBuf _addrLatch
    _dataOut = Nothing
    _interruptAck = False

type M = MaybeT (CPUM CPUState CPUOut)

pretty :: M String
pretty = do
    pc <- use pc
    sp <- use sp
    valueBuf <- use valueBuf
    addrBuf <- use addrBuf
    ~[bc, de, hl, af] <- mapM (use . MCPU.regPair . uncurry Regs) [(RB, RC), (RD, RE), (RH, RL), (RA, RFlags)]
    return $ unlines
      [ printf "IR:         PC: 0x%04x  SP: 0x%04x  U1:   0x%02x  U2: 0x%04x" pc sp valueBuf addrBuf
      , printf "BC: 0x%04x  DE: 0x%04x  HL: 0x%04x  AF: 0x%04x" bc de hl af
      ]

traceState :: (Show a) => M a -> M a
traceState act = do
    s <- pretty
    x <- act
    trace (unlines [s, show x]) $ return x

instance MCPU.MicroState CPUState where
    {-# INLINE reg #-}
    reg r = registers . lens (!! r) (\s v -> replace r v s)

    {-# INLINE pc #-}
    pc = pc

    {-# INLINE sp #-}
    sp = sp

    {-# INLINE valueBuf #-}
    valueBuf = valueBuf

    {-# INLINE addrBuf #-}
    addrBuf = addrBuf

instance MCPU.MicroM CPUState (MaybeT M) where
    {-# INLINE nextInstr #-}
    nextInstr = mzero

    {-# INLINE allowInterrupts #-}
    allowInterrupts = lift . assign allowInterrupts

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
    pc += 1
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
            nextInstr
        Fetching False | interrupted -> do
            acceptInterrupt
            phase .= Fetching True
        Fetching interrupting -> do
            instr <- {- traceState $ -} if interrupting then readByte inp else fetch inp
            let (setup, _) = microcodeFor instr
            load <- addressing (wedgeRight setup)
            phase .= Executing instr load 0
        Executing instr load i -> do
            when load $ assign valueBuf =<< readFrom dataIn

            let (uop, teardown) = snd (microcodeFor instr) !! i
            -- traceShow (i, uop, teardown) $ return ()
            x <- runMaybeT $ MCPU.uexec uop
            case x of
                Nothing -> do
                    nextInstr
                Just () -> do
                    load' <- addressing teardown
                    maybe nextInstr (assign phase . Executing instr load') $ succIdx i

nextInstr :: M ()
nextInstr = do
    latchAddr =<< use pc
    phase .= Fetching False

addressing :: Wedge OutAddr InAddr -> M Bool
addressing Nowhere = return False
addressing (Here write) = do
    doWrite =<< MCPU.outAddr write
    return False
addressing (There read) = do
    doRead =<< MCPU.inAddr read
    return True

doWrite :: Either Port Addr -> M ()
doWrite target = do
    addrOut .:= target
    value <- use valueBuf
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
