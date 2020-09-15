{-# LANGUAGE GADTs, ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}

{-# LANGUAGE RankNTypes #-} -- Needed for `step` only
{-# LANGUAGE UndecidableInstances #-} -- Needed for the `TypeError` instance of `Combine` only

-- | A container for sequences where each step can have a pre- and a
-- postamble. The postamble and the preamble of two neighbouring steps
-- are collapsed.
module Hardware.Intel8080.Amble
       ( Step(..)
       , step
       , Ends(..), Amble(End), (>:>), (>++>)
       , stepsOf
       ) where

import Prelude ()
import Clash.Prelude
import Data.Singletons
import Data.Kind (Constraint)

type ConflictErr post pre =
    Text "Conflict between postamble" :$$: Text "  " :<>: ShowType post :$$:
    Text "and next preamble" :$$: Text "  " :<>: ShowType pre

type family Combine (post :: Maybe b) (pre :: Maybe a) :: Maybe (Either a b) where
    Combine Nothing Nothing = Nothing
    Combine (Just post) Nothing = Just (Right post)
    Combine Nothing (Just pre) = Just (Left pre)
    Combine (Just post) (Just pre) = TypeError (ConflictErr post pre)

combine
    :: forall a b (post :: Maybe b) (pre :: Maybe a).
       (SingKind a, SingKind b, SingI (Combine post pre))
    => Sing post -> Sing pre -> Demote (KindOf (Combine post pre))
combine _ _ = demote @(Combine post pre)

data Step (pre :: Maybe a) (post :: Maybe b) t where
    Step :: Sing pre -> t -> Sing post -> Step pre post t
deriving instance Functor (Step pre post)

step :: forall pre. (SingI pre) => forall t. t -> forall post. (SingI post) => Step pre post t
step x = Step sing x sing

data Ends a b
    = Empty
    | NonEmpty (Maybe a) (Maybe b)

data Amble (n :: Nat) (ends :: Ends a b) t where
    End :: Amble 0 Empty t
    More
        :: forall (a0 :: Maybe a) (bn :: Maybe b) n t. ()
        => Sing a0
        -> Vec n (t, Demote (Maybe (Either a b)))
        -> t
        -> Sing bn
        -> Amble (1 + n) (NonEmpty a0 bn) t
deriving instance Functor (Amble n ends)

type family CanCons (b1 :: Maybe b) (ends :: Ends a b) :: Constraint where
    CanCons b1 Empty = ()
    CanCons (b1 :: Maybe b) (NonEmpty a1 bn :: Ends a b) = (SingKind a, SingKind b, SingI (Combine b1 a1))

type family Cons (b1 :: Maybe b) (ends :: Ends a b) where
    Cons b1 Empty = b1
    Cons b1 (NonEmpty a1 bn) = bn

cons
    :: forall (a0 :: Maybe a) b1 n (ends :: Ends a b) t. (CanCons b1 ends)
    => Step a0 b1 t
    -> Amble n ends t
    -> Amble (1 + n) (NonEmpty a0 (Cons b1 ends)) t
cons (Step a0 x b1) End = More a0 Nil x b1
cons (Step a0 x b1) (More a1 xs xn bn) = More a0 ((x, combine b1 a1) :> xs) xn bn

infixr 5 >:>
(>:>)
    :: forall (a0 :: Maybe a) b1 n (ends :: Ends a b) t. (CanCons b1 ends)
    => Step a0 b1 t -> Amble n ends t -> Amble (1 + n) (NonEmpty a0 (Cons b1 ends)) t
(>:>) = cons

type family CanAppend (ends1 :: Ends a b) (ends2 :: Ends a b) :: Constraint where
    CanAppend (NonEmpty a1 bn) ends2 = CanCons bn ends2
    CanAppend ends1 ends2 = ()

type family Append (ends1 :: Ends a b) (ends2 :: Ends a b) where
    Append Empty ends2 = ends2
    Append ends1 Empty = ends1
    Append (NonEmpty a0 bn) (NonEmpty an bm) = NonEmpty a0 bm

append :: (CanAppend ends1 ends2) => Amble n ends1 t -> Amble m ends2 t -> Amble (n + m) (Append ends1 ends2) t
append End ys = ys
append (More a0 xs xn bn) End = More a0 xs xn bn
append (More a0 xs xn bn) (More an ys ym bm) = More a0 (xs ++ singleton (xn, combine bn an) ++ ys) ym bm

infixr 5 >++>
(>++>) :: (CanAppend ends1 ends2) => Amble n ends1 t -> Amble m ends2 t -> Amble (n + m) (Append ends1 ends2) t
(>++>) = append

stepsOf
    :: forall a b (ends :: Ends a b) n t. (SingKind a, SingKind b)
    => Amble n ends t
    -> (Maybe (Demote a), Vec n (t, Maybe (Demote (Either a b))))
stepsOf End = (Nothing, Nil)
stepsOf (More a0 xs xn bn) = (fromSing a0, xs :< (xn, Right <$> fromSing bn))
