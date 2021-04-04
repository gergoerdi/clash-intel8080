{-# LANGUAGE PartialTypeSignatures #-}
module Hardware.Intel8080.Microcode.Compress where

import Clash.Prelude hiding (fromList)
import Hardware.Intel8080
import Hardware.Intel8080.Microcode
import Data.Trie
import Hardware.Intel8080.Decode
import Data.Wedge

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Control.Monad.RWS
import Data.Ord (comparing)

links :: Trie k (NonEmpty a) -> [(Either a Int, k, Maybe (Either a Int))]
links t = snd $ execRWS (go t) Nothing 0
  where
    go = mapM_ node . children

    node (k, mx, t') = do
        next <- ask
        this <- case mx of
            Nothing -> Right <$> alloc
            Just (x:|xs) -> do
                tell [(Left x', k, next) | x' <- xs]
                return $ Left x
        tell [(this, k, next)]
        local (const $ Just this) $ go t'

    alloc = get <* modify succ

suffixTree :: (KnownNat n, Ord a) => Vec n (NonEmpty a) -> Trie a (NonEmpty (Index n))
suffixTree xs = fromListMany [ (NE.reverse word, i) | (i, word) <- toList $ zip indicesI xs ]

compress :: forall n a. (KnownNat n, Ord a) => Vec n (NonEmpty a) -> [(a, Maybe Int)]
compress = reorder . renumber . links . suffixTree
  where
    reorder = L.map snd . L.sortBy (comparing fst)

    renumber xs = [ (flat addr, (x, flat <$> next)) | (addr, x, next) <- xs ]
      where
        offset = snatToNum (SNat @n)

        flat (Left k) = fromIntegral k
        flat (Right idx) = idx + offset

microcodes :: Vec 256 (NonEmpty (MicroInstr, Wedge OutAddr InAddr))
microcodes = fmap (NE.fromList . truncate . toList) rawMicrocodes
  where
    truncate ((x, True):xs) = x : truncate xs
    truncate ((x, False):xs) = [x]
    truncate _ = []

rawMicrocodes :: Vec 256 MicroOps
rawMicrocodes = map (snd . microcode . decodeInstr . bitCoerce) indicesI
