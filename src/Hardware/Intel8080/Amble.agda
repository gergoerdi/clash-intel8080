open import Data.Maybe
open import Data.List
open import Data.Product
open import Data.Unit
open import Function
open import Data.Bool.Base using (T)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

module _ {Pre Post Meet : Set} (meet : Post → Pre → Maybe Meet) (injPost : Post → Meet) where

variable
  A : Set

data Step (A : Set) : Pre → Post → Set where
  step : (pre : Pre) → (x : A) → (post : Post) → Step A pre post

data Ends : Set where
  empty : Ends
  nonEmpty : (a₀ : Pre) → (bₙ : Post) → Ends

consE : Pre → Post → Ends → Ends
consE a₀ b₁ empty = nonEmpty a₀ b₁
consE a₀ b₁ (nonEmpty a₁ bₙ) = nonEmpty a₀ bₙ

consP : Post → Ends → Set
consP b₁ empty = ⊤
consP b₁ (nonEmpty a₁ bₙ) = T (is-just (meet b₁ a₁))

data Amble (A : Set) : Ends → Set where
  empty : Amble A empty
  cons : ∀ {a₀ b₁ ends} {prf : consP b₁ ends} → Step A a₀ b₁ → Amble A ends → Amble A (consE a₀ b₁ ends)

appendE : Ends → Ends → Ends
appendE empty ends2 = ends2
appendE (nonEmpty a₀ bₙ) empty = nonEmpty a₀ bₙ
appendE (nonEmpty a₀ bₙ) (nonEmpty aₙ bₘ) = nonEmpty a₀ bₘ

appendP : Ends → Ends → Set
appendP empty ends2 = ⊤
appendP (nonEmpty a₀ bₙ) ends2 = consP bₙ ends2

lemma : ∀ a₀ b₁ ends1 ends2 → appendE (consE a₀ b₁ ends1) ends2 ≡ consE a₀ b₁ (appendE ends1 ends2)
lemma a₀ b₁ empty empty = refl
lemma a₀ b₁ empty (nonEmpty a₁ bₙ) = refl
lemma a₀ b₁ (nonEmpty a₁ bₙ) empty = refl
lemma a₀ b₁ (nonEmpty a₁ bₙ) (nonEmpty a₂ bₙ₁) = refl

consP-append : ∀ {a₀ b₁} ends ends2 →
  consP b₁ ends →
  appendP (consE a₀ b₁ ends) ends2 →
  consP b₁ (appendE ends ends2)
consP-append empty ends2 consP appendP = appendP
consP-append (nonEmpty a₀ bₙ) empty consP appendP = consP
consP-append (nonEmpty a₀ bₙ) (nonEmpty a₁ bₙ₁) consP appendP = consP

appendP-append : ∀ {a₀ b₁} ends ends2 →
  consP b₁ ends →
  appendP (consE a₀ b₁ ends) ends2 →
  appendP ends ends2
appendP-append empty ends2 consP appendP = _
appendP-append (nonEmpty a₀ bₙ) ends2 consP appendP = appendP

append : ∀ {ends1 ends2} {prf : appendP ends1 ends2} →
  Amble A ends1 →
  Amble A ends2 →
  Amble A (appendE ends1 ends2)
append empty ys = ys
append {ends2 = ends2} {prf = appendP} (cons {a₀} {b₁} {ends} {prf = endP} x xs) ys
  rewrite lemma a₀ b₁ ends ends2 =
    cons {prf = consP-append ends ends2 endP appendP} x $
    append {prf = appendP-append ends ends2 endP appendP} xs ys

stepsOf : ∀ {ends} → Amble A ends → Maybe (Pre × List (A × Meet))
stepsOf empty = nothing
stepsOf (cons {prf = prf} (step pre x post) xs) = just (pre , go x post prf xs)
  where
    consP-cons : ∀ {post pre post′} ends → consP post (consE pre post′ ends) → T (is-just (meet post pre))
    consP-cons empty prf = prf
    consP-cons (nonEmpty a₀ bₙ) prf = prf

    go : ∀ {ends} → A → (post : Post) → (prf : consP post ends) → Amble A ends → List (A × Meet)
    go x post prf empty = [ x , injPost post ]
    go x post prf (cons {ends = ends} {prf = prf′} (step pre x′ post′) xs) = (x , to-witness-T (meet post pre) (consP-cons ends prf)) ∷ go x′ post′ prf′ xs
