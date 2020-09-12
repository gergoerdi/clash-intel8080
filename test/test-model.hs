{-# LANGUAGE RecordWildCards #-}

import Clash.Prelude hiding (lift)

import Hardware.Intel8080
import Hardware.Intel8080.TestBench
import Hardware.Intel8080.Model


import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Array.IO

import System.IO
import Data.Char (chr)

import Paths_intel8080

import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy (ByteString)


run :: IOArray Addr Value -> IO ByteString
run arr =
    fmap toLazyByteString . execWriterT $ runMaybeT $
    execStateT `flip` s0 $ runReaderT `flip` MkR{..} $
    forever $ runMaybeT $ unCPU step
  where
    readMem addr = liftIO $ readArray arr addr
    writeMem addr val = liftIO $ writeArray arr addr val

    inPort _ = return 0xff
    outPort port value = do
        case port of
            0x00 -> mzero
            0x01 -> do
                liftIO $ putChar . chr  . fromIntegral $ value
                tell $ word8 $ fromIntegral value
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
