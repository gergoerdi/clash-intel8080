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
import Data.ByteString.Builder
import Text.Printf

prelude :: [Value]
prelude = mconcat
    [ [ 0x3e, 0x0a ]        -- 0x0000: exit:    MVI A, 0x0a
    , [ 0xd3, 0x00 ]        -- 0x0002:          OUT 0
    , [ 0x76 ]              -- 0x0004:          HLT

    , [ 0x3e, 0x02 ]        -- 0x0005: message: MVI A, 0x02
    , [ 0xb9 ]              -- 0x0007:          CMP C
    , [ 0xc2, 0x0f, 0x00 ]  -- 0x0008:          JNZ 0x000f
    , [ 0x7b ]              -- 0x000B: putChr:  MOV A, E
    , [ 0xd3, 0x00 ]        -- 0x000C:          OUT 0
    , [ 0xc9 ]              -- 0x000E:          RET

    , [ 0x0e, 0x24 ]        -- 0x000F: putStr:  MVI C, '$'
    , [ 0x1a ]              -- 0x0011: loop:    LDAX DE
    , [ 0xb9 ]              -- 0x0012:          CMP C
    , [ 0xc2, 0x17, 0x00 ]  -- 0x0013:          JNZ next
    , [ 0xc9 ]              -- 0x0016:          RET
    , [ 0xd3, 0x00 ]        -- 0x0017: next:    OUT 0
    , [ 0x13 ]              -- 0x0019:          INX DE
    , [ 0xc3, 0x11, 0x00 ]  -- 0x001a:          JMP loop
    ]

testOutPort :: (MonadIO m, MonadWriter Builder m) => Bool -> Value -> m Value
testOutPort verbose value = do
    when verbose $ liftIO $ putChar . chr . fromIntegral $ value
    tell $ word8 . fromIntegral $ value
    return 0xff

banner :: (MonadIO m) => String -> m a -> m a
banner title act = do
    liftIO $ printf "\n%s> %s <%s\n" (L.replicate 10 '-') title (L.replicate 10 '-')
    x <- act
    liftIO $ printf "\n%s--%s--%s\n" (L.replicate 10 '-') ('-' <$ title) (L.replicate 10 '-')
    return x

load :: FilePath -> IO (IOArray Addr Value)
load romFile = do
    bs <- fmap fromIntegral . BS.unpack <$> BS.readFile romFile
    arr <- newArray (minBound, maxBound) 0x00
    zipWithM_ (writeArray arr) [0x0000..] prelude
    zipWithM_ (writeArray arr) [0x0100..] bs
    return arr

runTest :: (IOArray Addr Value -> IO a) -> FilePath -> IO a
runTest body romFile = do
    arr <- load romFile
    body arr
