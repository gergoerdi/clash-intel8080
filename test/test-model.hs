{-# LANGUAGE RecordWildCards #-}

import Prelude ((^))
import Clash.Prelude hiding ((^), lift)

import Hardware.Intel8080
import Hardware.Intel8080.TestBench
import Hardware.Intel8080.Model

import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Loops (whileM_)
import Data.Array.IO
import Data.IORef
import qualified Data.List as L
import qualified Data.ByteString as BS

import System.IO
import Data.Char (chr)

import Paths_intel8080

runTest romFile = banner romFile $ do
    romFile <- getDataFileName romFile
    bs <- BS.unpack <$> BS.readFile romFile
    let memL = L.take (2 ^ 16) $ prelude <> bs <> L.repeat 0x00
    (arr :: IOArray Addr Value) <- newListArray (minBound, maxBound) (fromIntegral <$> memL)

    let r = MkR{..}
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
    runMaybeT $ execRWST `flip` r `flip` s0 $ forever $ runMaybeT step

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    mapM_ runTest
      [ "image/testbench/TST8080.COM"
      , "image/testbench/8080PRE.COM"
      -- , "image/testbench/CPUTEST.COM"
      -- , "image/testbench/8080EXM.COM"
      ]
