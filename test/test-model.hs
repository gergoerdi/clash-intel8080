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

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import System.FilePath ((<.>))
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy (ByteString)

run :: Bool -> IOArray Addr Value -> IO ByteString
run verbose arr =
    fmap toLazyByteString . execWriterT $ runSoftCPU World{..} s0
  where
    readMem addr = liftIO $ readArray arr addr
    writeMem addr val = liftIO $ writeArray arr addr val
    inPort _ = return 0xff
    outPort _ = testOutPort verbose

    s0 = mkState 0x0100

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    defaultMain =<< goldenTests images
  where
    images =
        [ "image/testbench/TST8080.COM"
        , "image/testbench/8080PRE.COM"
          -- , "image/testbench/CPUTEST.COM"
          -- , "image/testbench/8080EXM.COM"
        ]

goldenTests :: [FilePath] -> IO TestTree
goldenTests images = do
    return $ testGroup "Intel 8080 test benches running on model"
      [ goldenVsString image (image <.> "out") (runTest (run False) image)
      | image <- images
      ]
