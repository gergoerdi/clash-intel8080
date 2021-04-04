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
import Control.Monad.Extra (whenM, whileM)
import Control.Lens hiding (index)
import Data.Wedge

mkState :: Addr -> MicroState
mkState = mkMicroState

data World m = World
    { readMem :: Addr -> m Value
    , writeMem :: Addr -> Value -> m ()
    , inPort :: Port -> m Value
    , outPort :: Port -> Value -> m Value
    }

newtype SoftCPU m a = SoftCPU
    { unSoftCPU :: ReaderT (World m) (StateT MicroState m) a }
    deriving newtype
      (Functor, Applicative, Monad,
       MonadReader (World m), MonadState MicroState)

runSoftCPU :: (Monad m) => World m -> MicroState -> m ()
runSoftCPU w s0 =
    (evalStateT `flip` s0) . (runReaderT `flip` w) .
    unSoftCPU $
    whileM nextInstr

instance MonadTrans SoftCPU where
    lift = SoftCPU . lift . lift

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

nextInstr :: (Monad m) => SoftCPU m Bool
nextInstr = do
    instr <- decodeInstr <$> fetchByte
    exec instr

interrupt :: (Monad m) => Instr -> SoftCPU m ()
interrupt instr = whenM (use allowInterrupts) $ do
    allowInterrupts .= False
    void $ exec instr

exec :: (Monad m) => Instr -> SoftCPU m Bool
exec instr = do
    let (setup, uops) = microcode instr
    addressing $ wedgeRight setup
    -- liftIO $ print (instr, uops)
    ex <- runExceptT $ mapM_ ustep uops
    return $ ex /= Left GotoHalt

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
doRead target = valueBuf <~ either readPort peekByte target
  where
    readPort port = do
        read <- asks inPort
        lift $ read port

ustep :: (Monad m) => MicroOp -> ExceptT FlowControl (SoftCPU m) ()
ustep (effect, post) = do
    uexec effect
    lift $ addressing post
