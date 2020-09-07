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
        [ [ 0xd3, 0x00 ]        -- 0x0000: OUT 0, A
        , [ 0x00, 0x00, 0x00 ]
        , [ 0xdb, 0x00 ]        -- 0x0005: IN A, 0
        , [ 0xc9 ]              -- 0x0007: RET
        ]

inTestPort :: (Addr -> IO Value) -> (Reg -> Value) -> Port -> IO (Maybe Value)
inTestPort readMem getReg port = fmap Just $ do
    case getReg rC of
        0x02 -> do -- Print character stored in E
            putChar . chr . fromIntegral $ getReg rE
        0x09 -> do -- Print from (DE) until '$'
            let start = bitCoerce (getReg rD, getReg rE)
                addrs = [start..]
            bs <- forWhileM addrs $ \addr -> do
                b <- readMem addr
                return $ guard (fromIntegral b /= ord '$') >> return b
            mapM_ (putChar . chr . fromIntegral) bs
        _ -> return ()
    return 0xff

outTestPort :: IO () -> Port -> Value -> IO (Maybe Value)
outTestPort finish _port _value = do
    finish
    return Nothing

banner :: (MonadIO m) => String -> m a -> m a
banner title act = do
    liftIO $ printf "\n%s> %s <%s\n" (L.replicate 10 '-') title (L.replicate 10 '-')
    x <- act
    liftIO $ printf "\n%s--%s--%s\n" (L.replicate 10 '-') ('-' <$ title) (L.replicate 10 '-')
    return x
