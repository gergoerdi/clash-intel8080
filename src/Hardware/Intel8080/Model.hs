{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
module Hardware.Intel8080.Model where

import Hardware.Intel8080
import Hardware.Intel8080.Decode
import Hardware.Intel8080.Microcode
import Hardware.Intel8080.MicroCPU

import Clash.Prelude hiding (lift)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
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

data World m = World
    { readMem :: Addr -> m Value
    , writeMem :: Addr -> Value -> m ()
    , inPort :: Port -> m Value
    , outPort :: Port -> Value -> m Value
    }

newtype SoftCPU m a = SoftCPU
    { unSoftCPU :: ReaderT (World m) (StateT MicroState (MaybeT m)) a }
    deriving newtype
      (Functor, Applicative, Alternative, Monad,
       MonadPlus, MonadReader (World m), MonadState MicroState)

runSoftCPU :: (Monad m) => World m -> MicroState -> m ()
runSoftCPU w s0 =
    void . runMaybeT .
    (execStateT `flip` s0) . (runReaderT `flip` w) .
    unSoftCPU $
    forever nextInstr

instance MonadTrans SoftCPU where
    lift = SoftCPU . lift . lift . lift

dumpState :: (MonadIO m) => SoftCPU m ()
dumpState = do
    str <- gets debugState
    lift . liftIO $ putStrLn str

fetchByte :: (Monad m) => SoftCPU m Value
fetchByte = do
    pc <- use pc <* (pc += 1)
    peekByte pc

peekByte :: (Monad m) => Addr -> SoftCPU m Value
peekByte addr = do
    readMem <- asks readMem
    lift $ readMem addr

nextInstr :: (Monad m) => SoftCPU m ()
nextInstr = do
    instr <- decodeInstr <$> fetchByte
    exec instr

interrupt :: (Monad m) => Instr -> SoftCPU m ()
interrupt instr = whenM (use allowInterrupts) $ do
    allowInterrupts .= False
    exec instr

exec :: (Monad m) => Instr -> SoftCPU m ()
exec instr = do
    let (setup, uops) = microcode instr
    addressing $ wedgeRight setup
    -- liftIO $ print (instr, uops)
    ex <- runExceptT $ mapM_ ustep uops
    case ex of
        Left GotoHalt -> mzero
        _ -> return ()

addressing :: (Monad m) => Wedge OutAddr InAddr -> SoftCPU m ()
addressing = bitraverse_ (doWrite <=< outAddr) (doRead <=< inAddr)

doWrite :: (Monad m) => Either Port Addr -> SoftCPU m ()
doWrite target = either writePort poke target =<< use valueBuf
  where
    writePort port value = do
        write <- asks outPort
        lift $ void $ write port value

    poke addr value = do
        writeMem <- asks writeMem
        lift $ writeMem addr value

doRead :: (Monad m) => Either Port Addr -> SoftCPU m ()
doRead target = assign valueBuf =<< either readPort peekByte target
  where
    readPort port = do
        read <- asks inPort
        lift $ read port

ustep :: (Monad m) => MicroOp -> ExceptT FlowControl (SoftCPU m) ()
ustep (effect, post) = do
    uexec effect
    lift $ addressing post
