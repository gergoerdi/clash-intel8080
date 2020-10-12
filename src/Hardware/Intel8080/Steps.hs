{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-} -- Needed for `TypeError` only

-- | A container for sequences where each step can have a pre- and a
-- postamble. The postamble and the preamble of two neighbouring steps
-- are collapsed.
module Hardware.Intel8080.Steps where

import Clash.Prelude
import Data.Wedge

data IMaybe (isJust :: Bool) a where
    INothing :: IMaybe False a
    IJust :: a -> IMaybe True a
deriving instance (Show a) => Show (IMaybe isJust a)

fromIMaybe :: IMaybe free a -> Maybe a
fromIMaybe INothing = Nothing
fromIMaybe (IJust x) = Just x

class Impossible where
    impossible :: a

type family Compat post1 pre2 where
    Compat True True = (TypeError (Text "Conflict between postamble and next preamble"), Impossible)
    Compat post1 pre2 = ()

data Step pre a post hasPre hasPost where
    Step :: IMaybe hasPre pre -> a -> IMaybe hasPost post -> Step pre a post hasPre hasPost
deriving instance (Show a, Show pre, Show post) => Show (Step pre a post hasPre hasPost)

data Steps pre a post (n :: Nat) hasPre hasPost where
    One :: Step pre a post hasPre hasPost -> Steps pre a post 1 hasPre hasPost
    More :: (Compat hasPost1 hasPre2) => Step pre a post hasPre1 hasPost1 -> Steps pre a post n hasPre2 hasPost2 -> Steps pre a post (1 + n) hasPre1 hasPost2

instance (Show a, Show pre, Show post) => Show (Steps pre a post hasPre n hasPost) where
   show (One p)  = show p <> "\n"
   show (More a b) = show a <> show b

step :: IMaybe hasPre pre -> a -> IMaybe hasPost post -> Steps pre a post 1 hasPre hasPost
step hasPre x hasPost = One $ Step hasPre x hasPost

infixr 5 >++>
(>++>)
    :: (Compat hasPost1 hasPre2)
    => Steps pre a post n hasPre1 hasPost1
    -> Steps pre a post k hasPre2 hasPost2
    -> Steps pre a post (n + k) hasPre1 hasPost2
One x >++> ys = More x ys
More x xs >++> ys = More x $ xs >++> ys

stepsOf :: Steps pre a post n hasPre hasPost -> (Maybe pre, Vec n (a, Wedge post pre))
stepsOf xs = case go xs of (hasPre, ys) -> (fromIMaybe hasPre, ys)
  where
    go :: Steps pre a post n hasPre hasPost -> (IMaybe hasPre pre, Vec n (a, Wedge post pre))
    go (One (Step pre x post)) =
        (pre, singleton (x, wedgeLeft $ fromIMaybe post))
    go (More (Step pre x post) xs) =
        let (pre', ys) = go xs
            combined = case (post, pre') of
                (INothing, pre) -> wedgeRight $ fromIMaybe pre
                (post, INothing) -> wedgeLeft $ fromIMaybe post
                (IJust{}, IJust{}) -> impossible
        in (pre, (x, combined) :> ys)
