{-# LANGUAGE RecordWildCards #-}
import Hardware.Intel8080
import Hardware.Intel8080.TestBench
import Hardware.Intel8080.CPU
import Hardware.Intel8080.Sim

import Clash.Prelude hiding (lift, (^))
import Prelude ((^))

import Control.Monad.State
import Control.Monad.Loops (whileM_)
import Control.Lens hiding (Index)

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
    let mkWorld i s = World{..}
          where
            readMem_ = fmap fromIntegral . readArray arr . fromIntegral
            readMem addr = do
                if i `elem` [0, 1, 2, 3, 7] then Just <$> readMem_ addr
                  else return Nothing
            writeMem addr = writeArray arr (fromIntegral addr) . fromIntegral
            inPort r = Just <$> inTestPort readMem_ (_registers s !!) r
            outPort r x = Just <$> outTestPort (writeIORef finished True) r x

    let runSim act = evalStateT act ((0 :: Unsigned 3), (initInput, initState{ _pc = 0x0100 }, Nothing))
    runSim $ whileM_ (liftIO $ not <$> readIORef finished) $ do
    -- runSim $ replicateM_ 15 $ do
        i <- use _1
        zoom _2 $ sim (mkWorld i)
        _1 += 1

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
