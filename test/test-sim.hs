{-# LANGUAGE RecordWildCards, BangPatterns #-}

import Hardware.Intel8080
import Hardware.Intel8080.TestBench
import Hardware.Intel8080.CPU
import Hardware.Intel8080.Sim

import Clash.Prelude hiding (lift, generate)

import Control.Monad.State
import Control.Monad.Writer
import Control.Lens hiding (Index, (<.>))
import Control.Monad.Trans.Maybe
import Control.Monad.Extra
import Control.Monad.Supply

import Data.Array.IO
import System.IO
import System.Environment
import Data.Char (chr)

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Test.QuickCheck
import System.FilePath ((<.>))
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy (ByteString)

run :: Bool -> IOArray Addr Value -> IO ByteString
run verbose arr = do
    accessPattern <- generate $ arbitrary `suchThat` or

    let runSim act =
            runStateT `flip` (Nothing, 0) $
            runStateT `flip` (initInput, initState 0x0100) $
            runSupplyT `flip` cycle accessPattern $
            act

    fmap toLazyByteString . execWriterT $ runSim $ whileM $ do
        memReady <- supply
        lift $ sim (mkWorld memReady)
  where
    mkWorld memReady = World{..}
      where
        readMem addr = do
            guard memReady
            liftIO $ readArray arr addr
        writeMem addr = liftIO . writeArray arr addr

        inPort port = do
            guard memReady
            return 0x00
        outPort _ = testOutPort verbose

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    args <- getArgs
    let images = if not (null args) then args else
            [ "image/testbench/TST8080.COM"
            , "image/testbench/8080PRE.COM"
            -- , "image/testbench/CPUTEST.COM"
            -- , "image/testbench/8080EXM.COM"
            ]

    defaultMain =<< goldenTests images

goldenTests :: [FilePath] -> IO TestTree
goldenTests images = do
    return $ testGroup "Intel 8080 test benches running on simulator"
      [ goldenVsString image (image <.> "out") (runTest (run False) image)
      | image <- images
      ]
