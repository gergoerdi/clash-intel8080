{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses #-}

module Hardware.Intel8080.Star
       ( Step(..)
       , Star(End), (>:>), (>++>)
       , stepsOf
       ) where

import Prelude ()
import Clash.Prelude
import Data.Singletons
import Data.Proxy
import Data.Singletons.TH (singletons)
import Data.Kind (Constraint)

data Step (before :: r) (after :: w) a where
    Step :: (SingI before, SingI after) => a -> Step before after a
deriving instance Functor (Step before after)

data Ends r w
    = Empty
    | NonEmpty r w

type family HeadC (c :: r -> Constraint) (ends :: Ends r w) :: Constraint where
    HeadC c Empty = ()
    HeadC c (NonEmpty r w) = c r

type family MidC (ends1 :: Ends r w) (c :: w -> r -> Constraint) (ends2 :: Ends r w) :: Constraint where
    MidC (NonEmpty r1 wn) c (NonEmpty rn wm) = c wn rn
    MidC _ c _ = ()

data Star (ends :: Ends (Maybe r) (Maybe w)) (n :: Nat) a where
    End :: Star Empty 0 a
    Snoc
        :: (SingKind r, SingKind w, SingI r0, SingI wn)
        => Vec n (a, Demote (Maybe (Either r w)))
        -> a
        -> Star (NonEmpty r0 wn :: Ends (Maybe r) (Maybe w)) (n + 1) a
deriving instance Functor (Star end n)

class (SingI (MeetOf a b)) => Meet (a :: Maybe w) (b :: Maybe r) where
    type MeetOf a b :: Maybe (Either r w)

instance Meet Nothing Nothing where
    type MeetOf Nothing Nothing = Nothing

instance (SingI a) => Meet (Just a) Nothing where
    type MeetOf (Just a) Nothing = Just (Right a)

instance (SingI b) => Meet Nothing (Just b) where
    type MeetOf Nothing (Just b) = Just (Left b)

class (HeadC (Meet w1) ends) => Cons (r0 :: Maybe r) (w1 :: Maybe w) (ends :: Ends (Maybe r) (Maybe w)) where
    type ConsOf r0 w1 ends :: Ends (Maybe r) (Maybe w)

instance Cons r w Empty where
    type ConsOf r w Empty = NonEmpty r w

instance (Meet w r1) => Cons r w (NonEmpty r1 wm) where
    type ConsOf r w (NonEmpty r1 wm) = NonEmpty r wm

cons
    :: forall r r0 w1 ends n a. (SingKind r, SingKind w, Cons r0 w1 ends)
    => Step (r0 :: Maybe r) (w1 :: Maybe w) a
    -> Star ends n a
    -> Star (ConsOf r0 w1 ends) (n + 1) a
cons (Step x) End = Snoc Nil x
-- cons (Step x) (Snoc xs xn :: Star (NonEmpty r1 wn) n a) = Snoc ((x, demote @(MeetOf w r1)) :> xs) xn
cons s@(Step x) ss@(Snoc xs xn) = Snoc ((x, after ss) :> xs) xn
  where
    after :: forall (r1 :: Maybe r) (wn :: Maybe w). (Cons r0 w1 (NonEmpty r1 wn)) => Star (NonEmpty r1 wn) n a -> Demote (Maybe (Either r w))
    after _ = demote @(MeetOf w1 r1)

infixr 5 >:>
(>:>) = cons

class (MidC ends1 Meet ends2) => Append (ends1 :: Ends (Maybe r) (Maybe w)) (ends2 :: Ends (Maybe r) (Maybe w)) where
    type AppendOf ends1 ends2 :: Ends (Maybe r) (Maybe w)

instance Append Empty ends2 where
    type AppendOf Empty ends2 = ends2

instance Append ends1 Empty where
    type AppendOf ends1 Empty = ends1

instance (Meet wn rn) => Append (NonEmpty r0 wn) (NonEmpty rn wm) where
    type AppendOf (NonEmpty r0 wn) (NonEmpty rn wm) = NonEmpty r0 wm

append
    :: forall rw (ends1 :: Ends (Maybe r) (Maybe w)) (ends2 :: Ends (Maybe r) (Maybe w)) n m a. (SingKind r, SingKind w, Append ends1 ends2)
    => Star ends1 n a
    -> Star ends2 m a
    -> Star (AppendOf ends1 ends2) (n + m) a
append End ss' = ss'
append ss End = ss
append ss@(Snoc xs xn) ss'@(Snoc ys ym) = Snoc (xs ++ singleton (xn, mid ss ss') ++ ys) ym
  where
    mid
        :: forall (r0 :: Maybe r) (wn :: Maybe w) (rn :: Maybe r) (wm :: Maybe w). (Meet wn rn)
        => Star (NonEmpty r0 wn) n a
        -> Star (NonEmpty rn wm) m a
        -> Demote (Maybe (Either r w))
    mid _ _ = demote @(MeetOf wn rn)

infixr 5 >++>
(>++>) = append

stepsOf'
    :: forall (ends :: Ends (Maybe r) (Maybe w)) n a.
       Star ends n a
    -> (Maybe (Demote r), Vec n (a, Maybe (Demote (Either r w))))
stepsOf' End = (Nothing, Nil)
stepsOf' ss@(Snoc xs xn) = (start ss, xs ++ singleton (xn, end ss))
  where
    start
        :: forall (r0 :: Maybe r) (wn :: Maybe w). (SingI r0)
        => Star (NonEmpty r0 wn) n a
        -> Demote (Maybe r)
    start _ = demote @r0

    end
        :: forall (r0 :: Maybe r) (wn :: Maybe w). (SingI wn)
        => Star (NonEmpty r0 wn) n a
        -> Demote (Maybe (Either r w))
    end _ = Right <$> demote @wn

stepsOf
    :: forall (ends :: Ends (Maybe rw) (Maybe rw)) n a.
       Star ends n a
    -> (Maybe (Demote rw), Vec n (a, Maybe (Demote rw)))
stepsOf = fmap (map . fmap . fmap $ either id id) . stepsOf'
