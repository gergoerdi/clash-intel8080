{-# LANGUAGE RecordWildCards #-}
module Hardware.Clash.Intel8080.TestBench where

import Hardware.Intel8080
import Hardware.Intel8080.TestBench
import Hardware.Clash.Intel8080.Sim
import Hardware.Clash.Intel8080.CPU
import Hardware.Emulator.Memory as Mem

import Clash.Prelude hiding ((!), delay, lift, (^))
import Prelude ((^))
import Control.Lens hiding (index)

import Control.Monad.RWS
import Control.Monad.Loops (whileM_)

import qualified Data.ByteString as BS
import qualified Data.List as L
import Text.Printf
import Data.Array.IO
import Data.IORef
import System.IO

runTest romFile = do
    printf "Running tests from image %s\n" romFile

    bs <- BS.unpack <$> BS.readFile romFile
    let memL = L.take (2 ^ 16) $ prelude <> bs <> L.repeat 0x00
    memArr <- newListArray (minBound, maxBound) (fromIntegral <$> memL)
    let mem = ram (memArr :: IOArray Addr Value)

    let readMem = peekAt mem
        writeMem = pokeTo mem

    finished <- newIORef False
    let inPort s = inTestPort (peekAt mem) (registers s !!)
        outPort s = outTestPort (writeIORef finished True)

    let runSim act = void $ evalRWST act MkSimR{..} (initSim, initState{ pc = 0x0100 })

    runSim $ whileM_ (liftIO $ not <$> readIORef finished) $ do
        sim cpu
    putStrLn ""

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    mapM_ runTest
      [ "image/testbench/TST8080.COM"
      , "image/testbench/8080PRE.COM"
      , "image/testbench/CPUTEST.COM"
      -- , "image/testbench/8080EXM.COM"
      ]
