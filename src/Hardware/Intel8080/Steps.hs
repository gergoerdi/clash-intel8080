{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-} -- Needed for `TypeError` only

-- | A container for sequences where each step can have a pre- and a
-- postamble. The postamble and the preamble of two neighbouring steps
-- are collapsed.
module Hardware.Intel8080.Steps where

import Clash.Prelude
import Data.Singletons.Prelude.Bool

data IMaybe (isJust :: Bool) a where
    INothing :: IMaybe False a
    IJust :: a -> IMaybe True a
deriving instance (Show a) => Show (IMaybe isJust a)

class Impossible where
    impossible :: a

type family Compat post1 pre2 where
    Compat True True = (TypeError (Text "Conflict between postamble and next preamble"), Impossible)
    Compat post1 pre2 = ()

data Step b1 a b2 pre post where
    Step :: IMaybe pre b1 -> a -> IMaybe post b2 -> Step b1 a b2 pre post

data Steps b1 a b2 (n :: Nat) pre post where
    Pure :: Step b1 a b2 pre post -> Steps b1 a b2 1 pre post
    Cat
        :: (Compat post1 pre2)
        => Steps b1 a b2 (1 + n)         pre1 post1
        -> Steps b1 a b2 (1 + k)         pre2 post2
        -> Steps b1 a b2 (1 + n + 1 + k) pre1 post2

step :: IMaybe pre b1 -> a -> IMaybe post b2 -> Steps b1 a b2 1 pre post
step pre x post = Pure $ Step pre x post

infixr 5 >++>
(>++>)
    :: (Compat post1 pre2)
    => Steps b1 a b2 (1 + n) pre1 post1
    -> Steps b1 a b2 (1 + k) pre2 post2
    -> Steps b1 a b2 (1 + n + 1 + k) pre1 post2
(>++>) = Cat

deriving instance (Show a, Show b1, Show b2) => Show (Step b1 a b2 pre post)
instance (Show a, Show b1, Show b2) => Show (Steps b1 a b2 pre n post) where
   show (Pure p)  = show p <> "\n"
   show (Cat a b) = show a <> show b

stepsOf :: Steps b1 a b2 (1 + n) pre post -> (Maybe b1, Vec (1 + n) (a, Maybe (Either b2 b1)))
stepsOf ss = case flatten ss of
    (a0, xbs, xn, bn) -> (from a0, xbs :< (xn, Left <$> from bn))
  where
    flatten :: Steps b1 a b2 (1 + n) pre post -> (IMaybe pre b1, Vec n (a, Maybe (Either b2 b1)), a, IMaybe post b2)
    flatten (Pure (Step pre x post)) = (pre, Nil, x, post)
    flatten (Cat s1 s2) = case (flatten s1, flatten s2) of
        ((a0, xbs, xn, bn), (an, ybs, ym, bm)) -> (a0, xbs ++ singleton (xn, combine bn an) ++ ybs, ym, bm)

    combine :: (Compat post1 pre2) => IMaybe post1 b2 -> IMaybe pre2 b1 -> Maybe (Either b2 b1)
    combine INothing post = Right <$> from post
    combine pre INothing = Left <$> from pre
    combine IJust{} IJust{} = impossible

    from :: IMaybe free m -> Maybe m
    from INothing = Nothing
    from (IJust x) = Just x
