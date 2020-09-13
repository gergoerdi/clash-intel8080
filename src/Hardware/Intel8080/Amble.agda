open import Data.Maybe
open import Data.List
open import Data.Product
open import Data.Unit
open import Function
open import Data.Bool.Base using (T)

module _ {Pre Post Meet : Set} (meet : Post → Pre → Maybe Meet) (injPost : Post → Meet) where

variable
  A : Set

data Step A : Pre → Post → Set where
  step : (pre : _) → (x : A) → (post : _) → Step A pre post

data Ends : Set where
  empty : Ends
  nonEmpty : (a₀ : Pre) → (bₙ : Post) → Ends

consE : Pre → Post → Ends → Ends
consE a₀ b₁ empty = nonEmpty a₀ b₁
consE a₀ b₁ (nonEmpty a₁ bₙ) = nonEmpty a₀ bₙ

consP : Post → Ends → Set
consP b₁ empty = ⊤
consP b₁ (nonEmpty a₁ bₙ) = T (is-just (meet b₁ a₁))

data Amble A : Ends → Set where
  empty : Amble A empty
  nonEmpty : ∀ (a₀ : Pre) → (x : A) → List (Meet × A) → (bₙ : Post) → Amble A (nonEmpty a₀ bₙ)

cons : ∀ {a₀ b₁ ends} {prf : consP b₁ ends} → Step A a₀ b₁ → Amble A ends → Amble A (consE a₀ b₁ ends)
cons (step a₀ x b₁) empty = nonEmpty a₀ x [] b₁
cons {prf = prf} (step a₀ x b₁) (nonEmpty a₁ x₁ xs bₙ) = nonEmpty a₀ x ((to-witness-T (meet b₁ a₁) prf , x₁) ∷ xs) bₙ

appendE : Ends → Ends → Ends
appendE empty ends2 = ends2
appendE (nonEmpty a₀ bₙ) empty = nonEmpty a₀ bₙ
appendE (nonEmpty a₀ bₙ) (nonEmpty aₙ bₘ) = nonEmpty a₀ bₘ

appendP : Ends → Ends → Set
appendP empty ends2 = ⊤
appendP (nonEmpty a₀ bₙ) ends2 = consP bₙ ends2

append : ∀ {ends1 ends2} {prf : appendP ends1 ends2} → Amble A ends1 → Amble A ends2 → Amble A (appendE ends1 ends2)
append empty ys = ys
append (nonEmpty a₀ x xs bₙ) empty = nonEmpty a₀ x xs bₙ
append {prf = prf} (nonEmpty a₀ x xs bₙ) (nonEmpty aₙ y ys bₘ) =
  nonEmpty a₀ x (xs ++ (to-witness-T (meet bₙ aₙ) prf , y) ∷ ys) bₘ

stepsOf : ∀ {ends} → Amble A ends → Maybe (Pre × List (A × Meet))
stepsOf empty = nothing
stepsOf (nonEmpty a₀ x xs bₙ) = just (a₀ , go x xs)
  where
    go : A → List (Meet × A) → List (A × Meet)
    go x [] = [ (x , injPost bₙ) ]
    go x ((ab₁ , x′) ∷ xs) = (x , ab₁) ∷ go x′ xs
