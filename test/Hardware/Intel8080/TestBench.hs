module Hardware.Intel8080.TestBench where

import Clash.Prelude
import Hardware.Intel8080

import Prelude (putChar)
import Control.Monad
import Control.Monad.IO.Class
import Data.Array
import Data.Char
import Data.Word (Word8)
import qualified Data.List as L
import qualified Data.ByteString as BS
import Text.Printf

-- TODO: Upgrade Clash so we have that...
instance (KnownNat n) => Ix (Unsigned n) where
    range (a, b) = [a..b]
    index (a, b) x = index (fromIntegral a, fromIntegral b) (fromIntegral x)
    inRange (a, b) x = inRange (fromIntegral a, fromIntegral b) (fromIntegral x)

mapWhileM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapWhileM f = go
  where
    go [] = return []
    go (x:xs) = do
        my <- f x
        case my of
            Nothing -> return []
            Just y -> (y:) <$> go xs

forWhileM :: (Monad m) => [a] -> (a -> m (Maybe b)) -> m [b]
forWhileM = flip mapWhileM

prelude :: [Word8]
prelude = L.take 0x100 $ framework <> L.repeat 0x00
  where
    framework = mconcat
        [ [ 0xd3, 0x00 ]        -- 0x0000: OUT 0
        , [ 0x00, 0x00, 0x00 ]
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

outTestPort :: IO () -> Port -> Value -> IO Value
outTestPort finish port value = do
    case port of
        0x00 -> finish
        0x01 -> putChar . chr . fromIntegral $ value
    return 0x00

banner :: (MonadIO m) => String -> m a -> m a
banner title act = do
    liftIO $ printf "\n%s> %s <%s\n" (L.replicate 10 '-') title (L.replicate 10 '-')
    x <- act
    liftIO $ printf "\n%s--%s--%s\n" (L.replicate 10 '-') ('-' <$ title) (L.replicate 10 '-')
    return x
