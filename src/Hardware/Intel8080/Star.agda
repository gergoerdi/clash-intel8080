{-# OPTIONS --type-in-type #-}
open import Data.Maybe
open import Data.List
open import Data.Product
open import Data.Unit
open import Data.Bool
open import Data.Nat
open import Function

module _ where

variable
  A R W : Set

data Step (A : Set) {R W : Set} : R → W → Set where
  step : (r : R) → (x : A) → (w : W) → Step A r w

data Ends (R W : Set) : Set where
  empty : Ends R W
  nonEmpty : (r₀ : R) → (wₙ : W) → Ends R W

data Meet {W R : Set} : Maybe W → Maybe R → Set where
  noRW  :         Meet nothing  nothing
  onlyW : ∀ {w} → Meet (just w) nothing
  onlyR : ∀ {r} → Meet nothing  (just r)

Cons : Maybe W → Ends (Maybe R) (Maybe W) → Set
Cons _ empty = ⊤
Cons w₀ (nonEmpty r₀ wₙ) = Meet w₀ r₀

ConsOf : (r : Maybe R) → {w : Maybe W} → {ends : Ends (Maybe R) (Maybe W)} → Cons w ends → Ends (Maybe R) (Maybe W)
ConsOf r {w}        {empty}                  tt    = nonEmpty r w
ConsOf r {.nothing} {(nonEmpty .nothing wₙ)} noRW  = nonEmpty r wₙ
ConsOf r {w}        {(nonEmpty .nothing wₙ)} onlyW = nonEmpty r wₙ
ConsOf r {.nothing} {(nonEmpty r₁ wₙ)}       onlyR = nonEmpty r wₙ

data Star (A R W : Set) : Ends R W → Set where
  empty : Star A R W empty
  snoc : (r₀ : R) → (xs : List (A × W)) → (x : A) → (wₙ : W) → Star A R W (nonEmpty r₀ wₙ)

cons : ∀ {r : Maybe R} {w : Maybe W} {ends} →
  (prf : Cons w ends) →
  Step A r w →
  Star A (Maybe R) (Maybe W) ends →
  Star A (Maybe R) (Maybe W) (ConsOf r prf)
cons tt (step r x w) empty = snoc r [] x w
cons noRW  (step r x .nothing) (snoc .nothing xs xₙ wₙ) = snoc r ((x , nothing) ∷ xs) xₙ wₙ
cons onlyW (step r x w)        (snoc .nothing xs xₙ wₙ) = snoc r ((x , w) ∷ xs) xₙ wₙ
cons onlyR (step r x .nothing) (snoc r₁ xs xₙ wₙ)       = snoc r ((x , nothing) ∷ xs) xₙ wₙ

Append : Ends (Maybe R) (Maybe W) → Ends (Maybe R) (Maybe W) → Set
Append empty ends2 = ⊤
Append (nonEmpty r₀ wₙ) empty = ⊤
Append (nonEmpty r₀ wₙ) (nonEmpty rₙ₊₁ wₘ) = Meet wₙ rₙ₊₁

AppendOf : {ends1 ends2 : Ends (Maybe R) (Maybe W)} → Append ends1 ends2 → Ends (Maybe R) (Maybe W)
AppendOf {ends1 = empty}                  {ends2 = ends2}                        tt    = ends2
AppendOf {ends1 = ends1@(nonEmpty r₀ wₙ)} {ends2 = empty}                        tt    = ends1
AppendOf {ends1 = nonEmpty r₀ .nothing}   {ends2 = nonEmpty .nothing wₘ}         noRW  = nonEmpty r₀ wₘ
AppendOf {ends1 = nonEmpty r₀ .(just _)}  {ends2 = nonEmpty .nothing wₘ}         onlyW = nonEmpty r₀ wₘ
AppendOf {ends1 = nonEmpty r₀ .nothing}   {ends2 = nonEmpty .(just _) wₘ}        onlyR = nonEmpty r₀ wₘ

append : ∀ {r : Maybe R} {w : Maybe W} {ends1 ends2} →
  (prf : Append ends1 ends2) →
  Star A (Maybe R) (Maybe W) ends1 →
  Star A (Maybe R) (Maybe W) ends2 →
  Star A (Maybe R) (Maybe W) (AppendOf prf)
append prf empty ss′ = ss′
append prf ss@(snoc r₀ xs x wₙ) empty = ss
append noRW  (snoc r₀ xs x .nothing)  (snoc .nothing ys y wₘ)  = snoc r₀ (xs ++ [ x , nothing ] ++ ys) y wₘ
append onlyW (snoc r₀ xs x (just wₙ)) (snoc .nothing ys y wₘ)  = snoc r₀ (xs ++ [ x , just wₙ ] ++ ys) y wₘ
append onlyR (snoc r₀ xs x .nothing)  (snoc .(just _) ys y wₘ) = snoc r₀ (xs ++ [ x , nothing ] ++ ys) y wₘ

stepsOf : ∀ {A R W ends} → Star A (Maybe R) W ends → Maybe R × List (A × W)
stepsOf empty = nothing , []
stepsOf (snoc r₀ xs x wₙ) = r₀ , xs ++ [ (x , wₙ) ]

module _ where
  data From : Set where
    fromA fromB : From

  data To : Set where
    toA toB : To

  ex2 : Step ℕ {R = Maybe From} {W = Maybe To} _ _
  ex2 = step nothing 42 (just toA)

  ex1 : Step ℕ {R = Maybe From} {W = Maybe To} _ _
  ex1 = step (just fromA) 11 nothing

  ss1 : Star ℕ (Maybe From) (Maybe To) _
  ss1 =
    cons noRW ex1 $
    cons _ ex2 $
    empty

  ex3 : Maybe From × List (ℕ × Maybe To)
  ex3 = stepsOf ss1
