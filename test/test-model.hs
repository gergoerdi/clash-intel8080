import Prelude ((^))
import Clash.Prelude hiding ((^))

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
import Text.Printf

runTest romFile = banner romFile $ do
    bs <- BS.unpack <$> BS.readFile romFile
    let memL = L.take (2 ^ 16) $ prelude <> bs <> L.repeat 0x00
    (arr :: IOArray Addr Value) <- newListArray (minBound, maxBound) (fromIntegral <$> memL)
    -- let mem = ram (memArr :: IOArray Addr Value)

    finished <- newIORef False
    let inPort s = inTestPort (readArray arr) (_registers s !!)
        outPort _ = outTestPort (writeIORef finished True)

    let stepTB act = do
            s <- get
            let r = MkR (readArray arr) (writeArray arr) (inPort s) (outPort s)
            (s, _) <- liftIO $ execRWST (runMaybeT act) r s
            put s

    let s = mkS{ _pc = 0x0100 }
    flip execStateT s $ whileM_ (liftIO $ not <$> readIORef finished) $ do
        stepTB step

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    mapM_ runTest
      [ "image/testbench/TST8080.COM"
      , "image/testbench/8080PRE.COM"
      , "image/testbench/CPUTEST.COM"
      -- , "image/testbench/8080EXM.COM"
      ]
