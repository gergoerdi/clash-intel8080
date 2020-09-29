{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
module Hardware.Intel8080.Model where

import Hardware.Intel8080
import Hardware.Intel8080.Decode
import Hardware.Intel8080.Microcode
import qualified Hardware.Intel8080.MicroCPU as MCPU

import Clash.Prelude hiding (lift)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.Bifoldable (bitraverse_)
import Control.Monad.Extra (whenM)
import Control.Lens hiding (index)
import Data.Wedge

import Debug.Trace
import Text.Printf

data S = MkS
    { _pc :: Addr
    , _sp :: Addr
    , _allowInterrupts :: Bool
    , _registers :: Vec 8 Value
    , _ureg1 :: Value
    , _ureg2 :: Addr
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

data R m = MkR
    { readMem :: Addr -> m Value
    , writeMem :: Addr -> Value -> m ()
    , inPort :: Port -> m Value
    , outPort :: Port -> Value -> m Value
    }

newtype CPU m a = CPU{ unCPU :: MaybeT (ReaderT (R m) (StateT S m)) a }
    deriving newtype
      (Functor, Applicative, Alternative, Monad, MonadFail,
       MonadPlus, MonadReader (R m), MonadState S)

instance MonadTrans CPU where
    lift = CPU . lift . lift . lift

instance MCPU.MicroState S where
    reg r = registers . lens (!! r) (\s v -> replace r v s)
    pc = pc
    sp = sp
    valueBuf = ureg1
    addrBuf = ureg2

instance (Monad m) => MCPU.MicroM S (CPU m) where
    nextInstr = mzero
    allowInterrupts = assign allowInterrupts

dumpState :: (MonadIO m) => CPU m ()
dumpState = do
    pc <- use pc
    sp <- use sp
    [bc, de, hl, af] <- mapM (use . MCPU.regPair . uncurry Regs) [(RB, RC), (RD, RE), (RH, RL), (RA, RFlags)]
    lift . liftIO $ do
        printf "IR:         PC: 0x%04x  SP: 0x%04x\n" pc sp
        printf "BC: 0x%04x  DE: 0x%04x  HL: 0x%04x  AF: 0x%04x\n" bc de hl af

fetchByte :: (Monad m) => CPU m Value
fetchByte = do
    pc <- use pc <* (pc += 1)
    peekByte pc

peekByte :: (Monad m) => Addr -> CPU m Value
peekByte addr = do
    readMem <- asks readMem
    lift $ readMem addr

step :: (Monad m) => CPU m ()
step = do
    instr <- decodeInstr <$> fetchByte
    exec instr

interrupt :: (Monad m) => Instr -> CPU m ()
interrupt instr = whenM (use allowInterrupts) $ do
    allowInterrupts .= False
    exec instr

exec :: (Monad m) => Instr -> CPU m ()
exec instr = do
    let (setup, uops) = microcode instr
    addressing $ wedgeRight setup
    -- liftIO $ print (instr, uops)
    mapM_ ustep uops

addressing :: (Monad m) => Wedge OutAddr InAddr -> CPU m ()
addressing = bitraverse_ (doWrite <=< MCPU.outAddr) (doRead <=< MCPU.inAddr)

doWrite :: (Monad m) => Either Port Addr -> CPU m ()
doWrite target = either writePort poke target =<< use ureg1
  where
    writePort port value = do
        write <- asks outPort
        lift $ void $ write port value

    poke addr value = do
        writeMem <- asks writeMem
        lift $ writeMem addr value

doRead :: (Monad m) => Either Port Addr -> CPU m ()
doRead target = assign ureg1 =<< either readPort peekByte target
  where
    readPort port = do
        read <- asks inPort
        lift $ read port

ustep :: (Monad m) => MicroOp -> CPU m ()
ustep (effect, post) = do
    MCPU.uexec effect
    addressing post
