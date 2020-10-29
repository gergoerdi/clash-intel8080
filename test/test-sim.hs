{-# LANGUAGE RecordWildCards, BangPatterns #-}

import Hardware.Intel8080
import Hardware.Intel8080.TestBench
import Hardware.Intel8080.CPU
import Hardware.Intel8080.Sim

import Clash.Prelude hiding (lift)

import Control.Monad.State
import Control.Monad.Writer
import Control.Lens hiding (Index, (<.>))
import Control.Monad.Trans.Maybe
import Control.Monad.Extra

import Data.Array.IO
import System.IO
import System.Environment
import Data.Char (chr)

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import System.FilePath ((<.>))
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy (ByteString)

run :: Bool -> IOArray Addr Value -> IO ByteString
run verbose arr = do
    let runSim act =
            runStateT `flip` Nothing $
            runStateT `flip` (initInput, initState 0x0100) $
            runStateT `flip` ((0 :: Unsigned 3)) $
            act

    fmap toLazyByteString . execWriterT $ runSim $ whileM $ do
        i <- get <* modify (+ 1)
        lift $ sim (mkWorld i)
  where
    counter = get <* modify (+ 1)

    mkWorld !i = World{..}
      where
        readMem addr = do
            guard $ i `elem` [0, 1, 2, 3, 7]
            liftIO $ readArray arr addr
        writeMem addr = liftIO . writeArray arr addr

        inPort port = return 0xff
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
