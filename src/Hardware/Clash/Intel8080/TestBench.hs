{-# LANGUAGE RecordWildCards #-}
module Hardware.Clash.Intel8080.TestBench where

import Hardware.Intel8080
import Hardware.Intel8080.TestBench
import Hardware.Clash.Intel8080.CPU
import Hardware.Clash.Intel8080.Sim

import Clash.Prelude hiding (lift, (^))
import Prelude ((^))
import Control.Lens hiding (index)

import Control.Monad.State
import Control.Monad.Loops (whileM_)

import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Word
import Text.Printf
import Data.Array.IO
import Data.IORef
import System.IO

runTest :: FilePath -> IO ()
runTest romFile = do
    printf "\n%s> %s <%s\n" (L.replicate 10 '-') romFile (L.replicate 10 '-')

    bs <- BS.unpack <$> BS.readFile romFile
    let memL = L.take (2 ^ 16) $ prelude <> bs <> L.repeat 0x00
    (arr :: IOUArray Word16 Word8) <- newListArray (minBound, maxBound) (fromIntegral <$> memL)

    finished <- newIORef False
    let w = World{..}
          where
            readMem = fmap fromIntegral . readArray arr . fromIntegral
            writeMem addr = writeArray arr (fromIntegral addr) . fromIntegral
            inPort s = inTestPort readMem (registers s !!)
            outPort s = outTestPort (writeIORef finished True)

    let runSim act = evalStateT act (Nothing, initInput, initState{ pc = 0x0100 })

    runSim $ whileM_ (liftIO $ not <$> readIORef finished) $ do
        inp <- use _2
        s <- use _3
        let (out, s') = runState (cpuMachine inp) s
        inp' <- zoom _1 $ world w s out
        _2 .= inp'
        _3 .= s'
    printf "\n%s--%s--%s\n" (L.replicate 10 '-') ('-' <$ romFile) (L.replicate 10 '-')

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    mapM_ runTest
      [ "image/testbench/TST8080.COM"
      , "image/testbench/8080PRE.COM"
      , "image/testbench/CPUTEST.COM"
      -- , "image/testbench/8080EXM.COM"
      ]
