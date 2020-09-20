module Hardware.Intel8080.Interruptor where

import Hardware.Intel8080 (Value, Interrupt)

import Clash.Prelude
import Data.Maybe (fromMaybe)
import RetroClash.Utils
import Control.Monad.State

rst :: Interrupt -> Value
rst v = bitCoerce (0b11 :: Unsigned 2, v, 0b111 :: Unsigned 3)

interruptor
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe Interrupt)
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom (Maybe Value))
interruptor irq ack = mealyStateB irqManager Nothing (irq, ack)
  where
    irqManager (irq, ack) = case irq of
        _ | ack -> do
            req <- gets $ fromMaybe 0
            put Nothing
            return (False, Just $ rst req)
        Just req -> do
            put $ Just req
            return (True, Nothing)
        Nothing -> do
            return (False, Nothing)
