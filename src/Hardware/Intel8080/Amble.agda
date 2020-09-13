open import Data.Maybe
open import Data.Nat
open import Data.Nat.Properties using (+-identityʳ)
open import Data.Vec
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

data Amble A : ℕ → Ends → Set where
  empty : Amble A 0 empty
  nonEmpty : ∀ {n} (a₀ : Pre) → (x : A) → Vec (Meet × A) n → (bₙ : Post) → Amble A (1 + n) (nonEmpty a₀ bₙ)

cons : ∀ {n a₀ b₁ ends} {prf : consP b₁ ends} → Step A a₀ b₁ → Amble A n ends → Amble A (1 + n) (consE a₀ b₁ ends)
cons (step a₀ x b₁) empty = nonEmpty a₀ x [] b₁
cons {prf = prf} (step a₀ x b₁) (nonEmpty a₁ x₁ xs bₙ) =
  nonEmpty a₀ x ((to-witness-T (meet b₁ a₁) prf , x₁) ∷ xs) bₙ

appendE : Ends → Ends → Ends
appendE empty ends2 = ends2
appendE (nonEmpty a₀ bₙ) empty = nonEmpty a₀ bₙ
appendE (nonEmpty a₀ bₙ) (nonEmpty aₙ bₘ) = nonEmpty a₀ bₘ

appendP : Ends → Ends → Set
appendP empty ends2 = ⊤
appendP (nonEmpty a₀ bₙ) ends2 = consP bₙ ends2

append : ∀ {n m ends1 ends2} {prf : appendP ends1 ends2} → Amble A n ends1 → Amble A m ends2 → Amble A (n + m) (appendE ends1 ends2)
append empty ys = ys
append {n = n} (nonEmpty a₀ x xs bₙ) empty rewrite +-identityʳ n = nonEmpty a₀ x xs bₙ
append {prf = prf} (nonEmpty a₀ x xs bₙ) (nonEmpty aₙ y ys bₘ) =
  nonEmpty a₀ x (xs ++ (to-witness-T (meet bₙ aₙ) prf , y) ∷ ys) bₘ

stepsOf : ∀ {n ends} → Amble A n ends → Maybe Pre × Vec (A × Meet) n
stepsOf empty = nothing , []
stepsOf (nonEmpty a₀ x xs bₙ) = just a₀ , go x xs
  where
    go : ∀ {n} → A → Vec (Meet × A) n → Vec (A × Meet) (1 + n)
    go x [] = [ (x , injPost bₙ) ]
    go x ((ab₁ , x′) ∷ xs) = (x , ab₁) ∷ go x′ xs
