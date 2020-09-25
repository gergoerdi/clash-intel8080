{-# LANGUAGE RecordWildCards #-}
import Clash.Prelude
import Clash.Annotations.TH

import Hardware.Intel8080
import Hardware.Intel8080.CPU

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Barbies
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad (void)

topEntity
    :: "CLK"      ::: Clock System
    -> "RESET"    ::: Reset System
    -> "DATA_IN"  ::: Signal System (Unsigned 8)
    -> "DATA_OUT" ::: Signal System (Unsigned 8)
topEntity = withEnableGen board
  where
    board dataIn0 = fromMaybe 0 <$> _dataOut
      where
        dataIn = Just <$> dataIn0
        interruptRequest = pure False
        CPUOut{..} = mealyCPU initState defaultOut (void . runMaybeT . cpu) CPUIn{..}
