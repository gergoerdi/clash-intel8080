{-# LANGUAGE RecordWildCards #-}
import Hardware.Intel8080
import Hardware.Intel8080.TestBench
import Hardware.Intel8080.CPU
import Hardware.Intel8080.Sim

import Clash.Prelude hiding (lift, (^))
import Prelude ((^))

import Control.Monad.State
import Control.Monad.Loops (whileM_)

import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Word
import Data.Array.IO
import Data.IORef
import System.IO
import System.Environment

runTest :: FilePath -> IO ()
runTest romFile = banner romFile $ do
    bs <- BS.unpack <$> BS.readFile romFile
    let memL = L.take (2 ^ 16) $ prelude <> bs <> L.repeat 0x00
    (arr :: IOUArray Word16 Word8) <- newListArray (minBound, maxBound) (fromIntegral <$> memL)

    finished <- newIORef False
    let mkWorld s = World{..}
          where
            readMem_ = fmap fromIntegral . readArray arr . fromIntegral
            readMem = fmap Just . readMem_
            writeMem addr = writeArray arr (fromIntegral addr) . fromIntegral
            inPort = inTestPort readMem_ (_registers s !!)
            outPort = outTestPort (writeIORef finished True)

    let runSim act = evalStateT act (initInput, initState{ _pc = 0x0100 }, Nothing)
    runSim $ whileM_ (liftIO $ not <$> readIORef finished) $ sim mkWorld

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    args <- getArgs
    let images = if not (null args) then args else
            [ "image/testbench/TST8080.COM"
            , "image/testbench/8080PRE.COM"
            , "image/testbench/CPUTEST.COM"
              -- , "image/testbench/8080EXM.COM"
            ]

    mapM_ runTest images
