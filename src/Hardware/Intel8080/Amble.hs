{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses #-}

-- | A container for sequences where each step can have a pre- and a
-- postamble. The postamble and the preamble of two neighbouring steps
-- are collapsed.
module Hardware.Intel8080.Amble
       ( Step(..)
       , Amble(End), (>:>), (>++>)
       , stepsOf
       ) where

import Prelude ()
import Clash.Prelude
import Data.Singletons
import Data.Proxy
import Data.Singletons.TH (singletons)
import Data.Kind (Constraint)

data Step (pre :: a) (post :: b) t where
    Step :: (SingI pre, SingI post) => t -> Step pre post t
deriving instance Functor (Step pre post)

data Ends a b
    = Empty
    | NonEmpty a b

type family HeadC (c :: a -> Constraint) (ends :: Ends a b) :: Constraint where
    HeadC c Empty = ()
    HeadC c (NonEmpty a b) = c a

type family MidC (ends1 :: Ends a b) (c :: b -> a -> Constraint) (ends2 :: Ends a b) :: Constraint where
    MidC (NonEmpty a1 bn) c (NonEmpty an bm) = c bn an
    MidC _ c _ = ()

data Amble (ends :: Ends (Maybe a) (Maybe b)) (n :: Nat) t where
    End :: Amble Empty 0 t
    Snoc
        :: (SingKind a, SingKind b, SingI a0, SingI bn)
        => Vec n (t, Demote (Maybe (Either a b)))
        -> t
        -> Amble (NonEmpty a0 bn :: Ends (Maybe a) (Maybe b)) (n + 1) t
deriving instance Functor (Amble end n)

class (SingI (MeetOf post pre)) => Meet (post :: Maybe b) (pre :: Maybe a) where
    type MeetOf post pre :: Maybe (Either a b)

instance Meet Nothing Nothing where
    type MeetOf Nothing Nothing = Nothing

instance (SingI post) => Meet (Just post) Nothing where
    type MeetOf (Just post) Nothing = Just (Right post)

instance (SingI pre) => Meet Nothing (Just pre) where
    type MeetOf Nothing (Just pre) = Just (Left pre)

class (HeadC (Meet b1) ends) => Cons (a0 :: Maybe a) (b1 :: Maybe b) (ends :: Ends (Maybe a) (Maybe b)) where
    type ConsOf a0 b1 ends :: Ends (Maybe a) (Maybe b)

instance Cons a0 b1 Empty where
    type ConsOf a0 b1 Empty = NonEmpty a0 b1

instance (Meet b1 a1) => Cons a0 b1 (NonEmpty a1 bm) where
    type ConsOf a0 b1 (NonEmpty a1 bm) = NonEmpty a0 bm

cons
    :: forall a b a0 b1 ends n t. (SingKind a, SingKind b, Cons a0 b1 ends)
    => Step (a0 :: Maybe a) (b1 :: Maybe b) t
    -> Amble ends n t
    -> Amble (ConsOf a0 b1 ends) (n + 1) t
cons (Step x) End = Snoc Nil x
-- cons (Step x) (Snoc xs xn :: Amble (NonEmpty r1 wn) n a) = Snoc ((x, demote @(MeetOf w r1)) :> xs) xn
cons s@(Step x) ss@(Snoc xs xn) = Snoc ((x, after ss) :> xs) xn
  where
    after :: forall (a1 :: Maybe a) (bn :: Maybe b). (Cons a0 b1 (NonEmpty a1 bn)) => Amble (NonEmpty a1 bn) n t -> Demote (Maybe (Either a b))
    after _ = demote @(MeetOf b1 a1)

infixr 5 >:>
(>:>) = cons

class (MidC ends1 Meet ends2) => Append (ends1 :: Ends (Maybe a) (Maybe b)) (ends2 :: Ends (Maybe a) (Maybe b)) where
    type AppendOf ends1 ends2 :: Ends (Maybe a) (Maybe b)

instance Append Empty ends2 where
    type AppendOf Empty ends2 = ends2

instance Append ends1 Empty where
    type AppendOf ends1 Empty = ends1

instance (Meet bn an) => Append (NonEmpty a0 bn) (NonEmpty an bm) where
    type AppendOf (NonEmpty a0 bn) (NonEmpty an bm) = NonEmpty a0 bm

append
    :: forall rw (ends1 :: Ends (Maybe a) (Maybe b)) (ends2 :: Ends (Maybe a) (Maybe b)) n m t. (SingKind a, SingKind b, Append ends1 ends2)
    => Amble ends1 n t
    -> Amble ends2 m t
    -> Amble (AppendOf ends1 ends2) (n + m) t
append End ss' = ss'
append ss End = ss
append ss@(Snoc xs xn) ss'@(Snoc ys ym) = Snoc (xs ++ singleton (xn, mid ss ss') ++ ys) ym
  where
    mid
        :: forall (a0 :: Maybe a) (bn :: Maybe b) (an :: Maybe a) (bm :: Maybe b). (Meet bn an)
        => Amble (NonEmpty a0 bn) n t
        -> Amble (NonEmpty an bm) m t
        -> Demote (Maybe (Either a b))
    mid _ _ = demote @(MeetOf bn an)

infixr 5 >++>
(>++>) = append

stepsOf
    :: forall (ends :: Ends (Maybe a) (Maybe b)) n t.
       Amble ends n t
    -> (Maybe (Demote a), Vec n (t, Maybe (Demote (Either a b))))
stepsOf End = (Nothing, Nil)
stepsOf ss@(Snoc xs xn) = (start ss, xs ++ singleton (xn, end ss))
  where
    start
        :: forall (a0 :: Maybe a) (bn :: Maybe b). (SingI a0)
        => Amble (NonEmpty a0 bn) n t
        -> Demote (Maybe a)
    start _ = demote @a0

    end
        :: forall (a0 :: Maybe a) (bn :: Maybe b). (SingI bn)
        => Amble (NonEmpty a0 bn) n t
        -> Demote (Maybe (Either a b))
    end _ = Right <$> demote @bn
