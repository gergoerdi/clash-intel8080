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

import Data.Array.IO
import System.IO
import System.Environment
import Data.Char (chr)

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import System.FilePath ((<.>))
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy (ByteString)

run :: IOArray Addr Value -> IO ByteString
run arr = do
    let runSim act = evalStateT act ((0 :: Unsigned 3), (initInput, initState 0x0100, Nothing))

    fmap toLazyByteString . execWriterT $ runMaybeT $ runSim $ forever $ do
        i <- use _1
        zoom _2 $ sim (mkWorld i)
        _1 += 1
  where
    mkWorld !i s = World{..}
      where
        readMem addr = do
            if i `elem` [0, 1, 2, 3, 7] then Just <$> readMem_ addr
              else return Nothing
        readMem_ = liftIO . readArray arr
        writeMem addr = liftIO . writeArray arr addr

        inPort port = Just <$> return 0xff
        outPort port value = do
            case port of
                0x00 -> do
                    liftIO $ putStrLn ""
                    tell $ char7 '\n'
                    mzero
                0x01 -> do
                    liftIO $ putChar . chr  . fromIntegral $ value
                    tell $ word8 . fromIntegral $ value
            return $ Just 0xff


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

    -- mapM_ (runTest run) images
    defaultMain =<< goldenTests images

goldenTests :: [FilePath] -> IO TestTree
goldenTests images = do
    return $ testGroup "Intel 8080 test benches running on model"
      [ goldenVsString image (image <.> "out") (runTest run image)
      | image <- images
      ]
