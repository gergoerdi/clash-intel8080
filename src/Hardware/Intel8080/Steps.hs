{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-} -- Needed for `TypeError` only

-- | A container for sequences where each step can have a pre- and a
-- postamble. The postamble and the preamble of two neighbouring steps
-- are collapsed.
module Hardware.Intel8080.Steps where

import Clash.Prelude

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
    One :: Step b1 a b2 pre post -> Steps b1 a b2 1 pre post
    More :: (Compat post1 pre2) => Step b1 a b2 pre1 post1 -> Steps b1 a b2 n pre2 post2 -> Steps b1 a b2 (1 + n) pre1 post2

step :: IMaybe pre b1 -> a -> IMaybe post b2 -> Steps b1 a b2 1 pre post
step pre x post = One $ Step pre x post

infixr 5 >++>
(>++>)
    :: (Compat post1 pre2)
    => Steps b1 a b2 n pre1 post1
    -> Steps b1 a b2 k pre2 post2
    -> Steps b1 a b2 (n + k) pre1 post2
One x >++> ys = More x ys
More x xs >++> ys = More x $ xs >++> ys

deriving instance (Show a, Show b1, Show b2) => Show (Step b1 a b2 pre post)
instance (Show a, Show b1, Show b2) => Show (Steps b1 a b2 pre n post) where
   show (One p)  = show p <> "\n"
   show (More a b) = show a <> show b

stepsOf :: Steps b1 a b2 n pre post -> (Maybe b1, Vec n (a, Maybe (Either b2 b1)))
stepsOf xs = case go xs of (pre, ys) -> (from pre, ys)
  where
    go :: Steps b1 a b2 n pre post -> (IMaybe pre b1, Vec n (a, Maybe (Either b2 b1)))
    go (One (Step pre x post)) = (pre, singleton (x, Left <$> from post))
    go (More (Step pre x post) xs) = case go xs of (pre', ys) -> (pre, (x, combine post pre') :> ys)

    combine :: (Compat post1 pre2) => IMaybe post1 b2 -> IMaybe pre2 b1 -> Maybe (Either b2 b1)
    combine INothing post = Right <$> from post
    combine pre INothing = Left <$> from pre
    combine IJust{} IJust{} = impossible

    from :: IMaybe free m -> Maybe m
    from INothing = Nothing
    from (IJust x) = Just x
