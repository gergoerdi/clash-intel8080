{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes #-}

-- | A container for sequences where each step can have a pre- and a
-- postamble. The postamble and the preamble of two neighbouring steps
-- are collapsed.
module Hardware.Intel8080.Amble
       ( Step(..)
       , step
       , Amble(End), (>:>), (>++>)
       , stepsOf
       ) where

import Prelude ()
import Clash.Prelude
import Data.Singletons
import Data.Proxy
import Data.Singletons.TH (singletons)
import Data.Kind (Constraint)

class (SingKind a, SingKind b, SingI (MeetOf post pre)) => Meet (post :: Maybe b) (pre :: Maybe a) where
    type MeetOf post pre :: Maybe (Either a b)
    meetOf :: Sing post -> Sing pre -> Demote (Maybe (Either a b)) -- MeetOf post pre)

instance (SingKind a, SingKind b) => Meet (Nothing :: Maybe b) (Nothing :: Maybe a) where
    type MeetOf Nothing Nothing = Nothing
    meetOf _ _ = Nothing

instance (SingKind a, SingKind b, SingI post) => Meet (Just post :: Maybe b) (Nothing :: Maybe a) where
    type MeetOf (Just post) Nothing = Just (Right post)
    meetOf post _ = Right <$> fromSing post

instance (SingKind a, SingKind b, SingI pre) => Meet (Nothing :: Maybe b) (Just pre :: Maybe a) where
    type MeetOf Nothing (Just pre) = Just (Left pre)
    meetOf _ pre = Left <$> fromSing pre

data Step (pre :: a) (post :: b) t where
    Step :: Sing pre -> t -> Sing post -> Step pre post t
deriving instance Functor (Step pre post)

step :: forall pre. (SingI pre) => forall t. t -> forall post. (SingI post) => Step pre post t
step x = Step sing x sing

data Ends a b
    = Empty
    | NonEmpty a b

data Amble (n :: Nat) (ends :: Ends (Maybe a) (Maybe b)) t where
    End :: Amble 0 Empty t
    More
        :: forall (a0 :: Maybe a) (bn :: Maybe b) n t.
          Sing a0
        -> t
        -> Vec n (Demote (Maybe (Either a b)), t)
        -> Sing bn
        -> Amble (1 + n) (NonEmpty a0 bn) t
deriving instance Functor (Amble n end)

type family HeadC (c :: a -> Constraint) (ends :: Ends a b) :: Constraint where
    HeadC c Empty = ()
    HeadC c (NonEmpty a b) = c a

class (HeadC (Meet b1) ends) => Cons (a0 :: Maybe a) (b1 :: Maybe b) (ends :: Ends (Maybe a) (Maybe b)) where
    type ConsOf a0 b1 ends :: Ends (Maybe a) (Maybe b)

instance Cons a0 b1 Empty where
    type ConsOf a0 b1 Empty = NonEmpty a0 b1

instance (Meet b1 a1, SingI a1) => Cons a0 b1 (NonEmpty a1 bm) where
    type ConsOf a0 b1 (NonEmpty a1 bm) = NonEmpty a0 bm

cons
    :: forall a0 b1 ends n t. (Cons a0 b1 ends)
    => Step a0 b1 t
    -> Amble n ends t
    -> Amble (1 + n) (ConsOf a0 b1 ends) t
cons (Step a0 x b1) End = More a0 x Nil b1
cons (Step a0 x b1) (More a1 x' xs bn) = More a0 x ((meetOf b1 a1, x') :> xs) bn

infixr 5 >:>
(>:>) = cons

type family MidC (ends1 :: Ends a b) (c :: b -> a -> Constraint) (ends2 :: Ends a b) :: Constraint where
    MidC (NonEmpty a1 bn) c (NonEmpty an bm) = c bn an
    MidC _ c _ = ()

class (MidC ends1 Meet ends2) => Append (ends1 :: Ends (Maybe a) (Maybe b)) (ends2 :: Ends (Maybe a) (Maybe b)) where
    type AppendOf ends1 ends2 :: Ends (Maybe a) (Maybe b)

instance Append Empty ends2 where
    type AppendOf Empty ends2 = ends2

instance Append ends1 Empty where
    type AppendOf ends1 Empty = ends1

instance (Meet bn an) => Append (NonEmpty a0 bn) (NonEmpty an bm) where
    type AppendOf (NonEmpty a0 bn) (NonEmpty an bm) = NonEmpty a0 bm

append :: (Append ends1 ends2) => Amble n ends1 t -> Amble m ends2 t -> Amble (n + m) (AppendOf ends1 ends2) t
append End ys = ys
append (More a0 x xs bn) End = More a0 x xs bn
append (More a0 x xs bn) (More an y ys bm) = More a0 x (xs ++ singleton (meetOf bn an, y) ++ ys) bm

infixr 5 >++>
(>++>) = append

stepsOf
    :: forall a b (ends :: Ends (Maybe a) (Maybe b)) n t.
       (SingKind a, SingKind b)
    => Amble n ends t
    -> (Maybe (Demote a), Vec n (t, Maybe (Demote (Either a b))))
stepsOf End = (Nothing, Nil)
stepsOf (More a0 x xs bn) = (fromSing a0, go x xs)
  where
    go :: forall k. t -> Vec k (Maybe (Demote (Either a b)), t) -> Vec (1 + k) (t, Maybe (Demote (Either a b)))
    go x Nil = singleton (x, Right <$> fromSing bn)
    go x (Cons (ab, x') xs) = Cons (x, ab) (go x' xs)
