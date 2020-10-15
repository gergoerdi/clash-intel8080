{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
module Hardware.Intel8080.Model where

import Hardware.Intel8080
import Hardware.Intel8080.Decode
import Hardware.Intel8080.Microcode
import Hardware.Intel8080.MicroCPU as MCPU

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

mkState :: Addr -> MicroState
mkState = mkMicroState

data R m = MkR
    { readMem :: Addr -> m Value
    , writeMem :: Addr -> Value -> m ()
    , inPort :: Port -> m Value
    , outPort :: Port -> Value -> m Value
    }

newtype CPU m a = CPU{ unCPU :: ReaderT (R m) (StateT MicroState m) a }
    deriving newtype
      (Functor, Applicative, Alternative, Monad, MonadFail,
       MonadPlus, MonadReader (R m), MonadState MicroState)

instance MonadTrans CPU where
    lift = CPU . lift . lift

dumpState :: (MonadIO m) => CPU m ()
dumpState = do
    pc <- use pc
    sp <- use sp
    ~[bc, de, hl, af] <- mapM (use . regPair . uncurry Regs) [(RB, RC), (RD, RE), (RH, RL), (RA, RFlags)]
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
    void $ runMaybeT $ mapM_ ustep uops

addressing :: (Monad m) => Wedge OutAddr InAddr -> CPU m ()
addressing = bitraverse_ (doWrite <=< outAddr) (doRead <=< inAddr)

doWrite :: (Monad m) => Either Port Addr -> CPU m ()
doWrite target = either writePort poke target =<< use valueBuf
  where
    writePort port value = do
        write <- asks outPort
        lift $ void $ write port value

    poke addr value = do
        writeMem <- asks writeMem
        lift $ writeMem addr value

doRead :: (Monad m) => Either Port Addr -> CPU m ()
doRead target = assign valueBuf =<< either readPort peekByte target
  where
    readPort port = do
        read <- asks inPort
        lift $ read port

ustep :: (Monad m) => MicroOp -> MaybeT (CPU m) ()
ustep (effect, post) = do
    uexec effect
    lift $ addressing post
