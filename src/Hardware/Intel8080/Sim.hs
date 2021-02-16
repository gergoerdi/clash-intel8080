{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Hardware.Intel8080.Sim
    ( World(..)
    , world
    , initInput
    , sim
    , interrupt
    ) where

import Hardware.Intel8080
import Hardware.Intel8080.Model (World(..))
import Hardware.Intel8080.CPU

import Clash.Prelude hiding (lift)

import RetroClash.Barbies
import RetroClash.CPU
import RetroClash.Utils

import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Foldable (traverse_, for_)
import Control.Lens hiding (index, Index)

data IRQ
    = NewIRQ Value
    | QueuedIRQ Value

initInput :: Pure CPUIn
initInput = CPUIn
    { pause = False
    , dataIn = Nothing
    , interruptRequest = False
    }

world :: (Monad m) => World (MaybeT m) -> Pure CPUOut -> StateT (Maybe IRQ, Index 7) m (Pure CPUIn)
world World{..} CPUOut{..} = do
    dataIn <- case _addrOut of
        Nothing ->
            return Nothing
        Just (Left port) ->
            lift . runMaybeT $ maybe (inPort port) (outPort port) _dataOut
        Just (Right addr) | _interruptAck ->
            getInterrupt
        Just (Right addr) -> lift $ do
            x <- runMaybeT $ readMem addr
            runMaybeT $ traverse_ (writeMem addr) _dataOut
            return x
    interruptRequest <- newInterrupt
    pause <- zoom _2 $ do
        x <- get <* modify nextIdx
        return $ x /= 0

    return CPUIn{..}
  where
    getInterrupt = zoom _1 $ get >>= \case
        Just (QueuedIRQ op) -> do
            put Nothing
            return $ Just op
        _ -> return $ Just 0x00

    newInterrupt = zoom _1 $ get >>= \case
        Just (NewIRQ op) -> do
            put $ Just $ QueuedIRQ op
            return True
        _ -> return False

sim :: (Monad m) => World (MaybeT m) -> StateT (Pure CPUIn, CPUState) (StateT (Maybe IRQ, Index 7) m) Bool
sim w = do
    (inp, s) <- get
    let (out, s') = runState (runCPU defaultOut $ cpu inp) s
    inp' <- lift $ world w out
    put (inp', s')
    return $ not $ _halted out

interrupt :: (Monad m) => Unsigned 3 -> StateT (Maybe IRQ) m ()
interrupt v = put $ Just $ NewIRQ rst
  where
    rst = bitCoerce (0b11 :: Unsigned 2, v, 0b111 :: Unsigned 3)
