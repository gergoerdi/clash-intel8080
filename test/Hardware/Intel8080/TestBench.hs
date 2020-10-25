module Hardware.Intel8080.TestBench where

import Clash.Prelude hiding ((^))
import Hardware.Intel8080

import Prelude (putChar, (^))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.Array
import Data.Array.IO
import Data.Char
import Data.Word (Word8)
import qualified Data.List as L
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Builder
import Text.Printf

prelude :: [Word8]
prelude = L.take 0x100 $ framework <> L.repeat 0x00
  where
    framework = mconcat
        [ [ 0xd3, 0x00 ]        -- 0x0000: OUT 0
        , [ 0x76 ]              -- 0x0002: HLT
        , [ 0x00, 0x00]

        , [ 0x3e, 0x02 ]        -- 0x0005: MVI A, 0x02
        , [ 0xb9 ]              -- 0x0007: CMP C
        , [ 0xc2, 0x0f, 0x00 ]  -- 0x0008: JNZ 0x000f
        , [ 0x7b ]              -- 0x000B: MOV A, E
        , [ 0xd3, 0x01 ]        -- 0x000C: OUT 1
        , [ 0xc9 ]              -- 0x000E: RET

        , [ 0x0e, 0x24 ]        -- 0x000F: MVI C, '$'
        , [ 0x1a ]              -- 0x0011: LDAX DE
        , [ 0xb9 ]              -- 0x0012: CMP C
        , [ 0xc2, 0x17, 0x00 ]  -- 0x0013: JNZ 0x0017
        , [ 0xc9 ]              -- 0x0016: RET
        , [ 0xd3, 0x01 ]        -- 0x0017: OUT 1
        , [ 0x13 ]              -- 0x0019: INX DE
        , [ 0xc3, 0x10, 0x00 ]  -- 0x001a: JMP 0x0011
        ]

testOutPort :: (MonadIO m, MonadWriter Builder m) => Bool -> Port -> Value -> m Value
testOutPort verbose port value = do
    case port of
        0x00 -> do
            when verbose $ liftIO $ putStrLn ""
            tell $ char7 '\n'
        0x01 -> do
            when verbose $ liftIO $ putChar . chr . fromIntegral $ value
            tell $ word8 . fromIntegral $ value
    return 0xff

banner :: (MonadIO m) => String -> m a -> m a
banner title act = do
    liftIO $ printf "\n%s> %s <%s\n" (L.replicate 10 '-') title (L.replicate 10 '-')
    x <- act
    liftIO $ printf "\n%s--%s--%s\n" (L.replicate 10 '-') ('-' <$ title) (L.replicate 10 '-')
    return x

runTest :: (IOArray Addr Value -> IO a) -> FilePath -> IO a
runTest body romFile = do
    bs <- BS.unpack <$> BS.readFile romFile
    let memL = L.take (2 ^ 16) $ prelude <> bs <> L.repeat 0x00
    arr <- newListArray (minBound, maxBound) (fromIntegral <$> memL)

    body arr
