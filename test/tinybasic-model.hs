import Prelude ((^))
import Clash.Prelude hiding ((^), lift)

import Hardware.Intel8080
import Hardware.Intel8080.TestBench ()
import Hardware.Intel8080.Model

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Array.IO
import qualified Data.List as L
import qualified Data.ByteString as BS

import Text.Printf
import Data.Char (chr, ord, isPrint)
import System.Terminal

import Paths_intel8080

waitKey :: (MonadInput m) => MaybeT m Value
waitKey = do
    ev <- lift awaitEvent
    case ev of
        Left _ -> mzero
        Right (KeyEvent (CharKey c) mods) | mods == mempty -> return $ fromIntegral . ord $ c
        Right (KeyEvent EnterKey _) -> return 0x0d
        _ -> waitKey

main :: IO ()
main = do
    romFile <- getDataFileName "image/tiny-basic/alpha-basic1000.a80.com"
    bs <- BS.unpack <$> BS.readFile romFile
    let memL = L.take (2 ^ 16) $ L.replicate 0x1000 0x00 <> bs <> L.repeat 0x00
    (arr :: IOArray Addr Value) <- newListArray (minBound, maxBound) (fromIntegral <$> memL)

    let verbose = False

    let getStatus = do
            let val = 0x03 :: Value
            when verbose $ liftIO $ printf "<- status 0x%02x\n" val
            return val
        putStatus val = do
            when verbose $ liftIO $ printf "-> status 0x%02x\n" val
            return 0x00

        getData = do
            when verbose $ putString "<- data "
            val <- fromMaybe (error "exit") <$> runMaybeT waitKey
            when verbose $ liftIO $ printf "0x%02x\n" val
            return val

        putData val = do
            when verbose $ liftIO $ printf "-> data 0x%02x\t" val
            let c = chr . fromIntegral $ val
            case val of
                0x0d -> putStringLn ""
                _ | isPrint c -> do
                    putChar c
                    when verbose $ putStringLn ""
                _ -> return ()
            return 0x00

    let inPort port
          | port == statusPort = getStatus
          | port == dataPort = getData
          | otherwise = return 0x00

        outPort port
          | port == statusPort = putStatus
          | port == dataPort = putData
          | otherwise = \_ -> return 0x00

    withTerminal $ runTerminalT $ do
        let r = MkR (liftIO . readArray arr) (\addr x -> liftIO $ writeArray arr addr x) inPort outPort
        let stepTB act = runReaderT `flip` r $ (runMaybeT $ unCPU act)

        let s = mkS{ _pc = 0x0100 }
        flip execStateT s $ forever $ stepTB step

    return ()
  where
    statusPort = 0xde
    dataPort = 0xdf
