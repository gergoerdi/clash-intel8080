open import Data.Maybe
open import Data.Vec
open import Data.Product
open import Data.Nat
open import Data.Sum
open import Function
open import Data.Bool.Base using (T)

import Amble

module _ where

  data Pre : Set where
    preA preB : Pre

  data Post : Set where
    postA postB : Post

  meet : Maybe Post → Maybe Pre → Maybe (Maybe (Pre ⊎ Post))
  meet nothing    post        = just (Data.Maybe.map inj₁ post)
  meet pre        nothing     = just (Data.Maybe.map inj₂ pre)
  meet (just pre) (just post) = nothing

  open Amble meet (Data.Maybe.map inj₂)

  ex2 : Step ℕ _ _
  ex2 = step nothing 42 (just postA)

  ex1 : Step ℕ _ _
  ex1 = step (just preA) 11 nothing

  ss1 : Amble ℕ 2 _
  ss1 =
    cons ex1 $
    cons ex2 $
    empty

  ex3 : Maybe (Maybe Pre) × Vec (ℕ × Maybe (Pre ⊎ Post)) 2
  ex3 = stepsOf ss1
