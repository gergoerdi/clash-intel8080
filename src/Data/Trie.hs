{-# LANGUAGE BlockArguments #-}
module Data.Trie where

import Prelude
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Foldable (forM_)
import Data.Maybe
import Text.Printf

newtype Trie k a = MkTrie{ childrenMap :: M.Map k (Maybe a, Trie k a) }
    deriving (Show)

children :: Trie k a -> [(k, Maybe a, Trie k a)]
children t = [(k, x, t') | (k, (x, t')) <- M.toList $ childrenMap t]

empty :: Trie k a
empty = MkTrie M.empty

insert :: (Ord k) => NonEmpty k -> a -> Trie k a -> Trie k a
insert ks x = insertOrUpdate (const x) ks

insertOrUpdate :: (Ord k) => (Maybe a -> a) -> NonEmpty k -> Trie k a -> Trie k a
insertOrUpdate f = go
  where
    go (k :| ks) (MkTrie ts) = MkTrie $ M.alter (Just . update . fromMaybe (Nothing, empty)) k ts
      where
        update (x, t) = case NE.nonEmpty ks of
            Nothing  -> (Just $ f x, t)
            Just ks' -> (x, go ks' t)

fromList :: (Ord k) => [(NonEmpty k, a)] -> Trie k a
fromList = foldr (uncurry insert) empty

fromListMany :: (Ord k) => [(NonEmpty k, a)] -> Trie k (NonEmpty a)
fromListMany = foldr (\(ks, x) -> insertOrUpdate ((x :|) . maybe [] NE.toList) ks) empty

printTrie :: (Show k, Show a) => Trie k a -> IO ()
printTrie = go 0
  where
    padding level = replicate (level * 2) ' '

    go :: (Show k, Show a) => Int -> Trie k a -> IO ()
    go level t = forM_ (children t) \(k, x, t') -> do
        printf "%s%s -> %s\n" (padding level) (show k) (maybe "_" show x)
        go (level + 1) t'
