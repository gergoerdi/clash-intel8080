{-# LANGUAGE RecordWildCards #-}

import Clash.Prelude hiding (lift)

import Hardware.Intel8080
import Hardware.Intel8080.TestBench
import Hardware.Intel8080.Model

import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Array.IO

import System.IO
import Data.Char (chr)

import Paths_intel8080

run :: IOArray Addr Value -> IO ()
run arr = void $ runMaybeT $ execRWST `flip` MkR{..} `flip` s0 $ forever $ runMaybeT step
  where
    readMem addr = liftIO $ readArray arr addr
    writeMem addr val = liftIO $ writeArray arr addr val

    inPort _ = return 0xff
    outPort port value = do
        case port of
            0x00 -> mzero
            0x01 -> liftIO $ putChar . chr  . fromIntegral $ value
        return 0xff

    s0 = mkS{ _pc = 0x0100 }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    mapM_ (runTest run)
      [ "image/testbench/TST8080.COM"
      , "image/testbench/8080PRE.COM"
      -- , "image/testbench/CPUTEST.COM"
      -- , "image/testbench/8080EXM.COM"
      ]
