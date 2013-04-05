data List ++(A : Set) : Set
{ nil 
; cons (head : A) (tail : List A) 
}

{- 2012-01-26
  data D {A : Set}
         (P : (A → Set₁) → Set₁)
         (Q : List A → Set) : Set₁ where
    d : P (λ x → D P (λ xs → Q (x ∷ xs))) → D P Q
-}

data D (A : Set) (P : ++(A -> Set 1) -> Set 1) (Q : List A -> Set) : Set 1
{ d : P (\ x -> D A P (\ xs -> Q (cons x xs))) -> D A P Q
} 

{-
If this type were accepted by Agda, then we could easily prove that the
empty type is inhabited (you can try by using --no-positivity-check):

  bad : ⊥
  bad = to-⊥ (d to-⊥)
    where
    to-⊥ : D (λ F → F tt → ⊥) (λ _ → ⊤) → ⊥
    to-⊥ (d f) = f (d f) 
-}

data Empty : Set {}
data Unit  : Set { tt }

trustme -- fails
fun toBot : D Unit (\ F -> F tt -> Empty) (\ xs -> Unit) -> Empty
{ toBot (d f) = f (d f)
}
-- error: F may not occur

let bad : Empty = toBot (d toBot) 
