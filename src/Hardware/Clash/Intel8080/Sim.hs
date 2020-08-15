{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Hardware.Clash.Intel8080.Sim where

import Hardware.Intel8080
import Hardware.Clash.Intel8080.CPU

import Clash.Prelude hiding (lift)

import RetroClash.Barbies
import RetroClash.CPU

import Control.Monad.State
import Data.Foldable (traverse_, for_)

data IRQ
    = NewIRQ Value
    | QueuedIRQ Value

data World m = World
    { readMem :: Addr -> m Value
    , writeMem :: Addr -> Value -> m ()
    , inPort :: CPUState -> Port -> m Value
    , outPort :: CPUState -> Port -> Value -> m Value
    }

initInput :: Pure CPUIn
initInput = CPUIn
    { cpuInMem = Nothing
    , cpuInIRQ = False
    }

world :: (Monad m) => World m -> CPUState -> Pure CPUOut -> StateT (Maybe IRQ) m (Pure CPUIn)
world World{..} s CPUOut{..} = do
    cpuInMem <- Just <$> read
    unless _cpuOutPortSelect $ lift $ traverse_ (writeMem _cpuOutMemAddr) _cpuOutMemWrite

    cpuInIRQ <- get >>= \case
        Just (NewIRQ op) -> do
            put $ Just $ QueuedIRQ op
            return True
        _ -> return False

    return CPUIn{..}
  where
    read | _cpuOutPortSelect = lift $ maybe (inPort s port) (outPort s port) _cpuOutMemWrite
         | _cpuOutIRQAck = get >>= \case
            Just (QueuedIRQ op) -> do
                put Nothing
                return op
            _ -> return 0x00
         | otherwise = lift $ readMem _cpuOutMemAddr

    port = truncateB _cpuOutMemAddr

interrupt :: (Monad m) => Unsigned 3 -> StateT (Maybe IRQ) m ()
interrupt v = put $ Just $ NewIRQ rst
  where
    rst = bitCoerce (0b11 :: Unsigned 2, v, 0b111 :: Unsigned 3)
