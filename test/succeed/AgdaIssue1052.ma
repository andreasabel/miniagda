
data Eq [A : Set] (a : A) (b : A) : Set
{ refl : Eq A a a }

fun X : Set {}
fun f : X -> X {}

data StepsTo (x, z : X) : Set
{ done : StepsTo x x
; next : (y : X) -> Eq X (f y) z -> StepsTo x y -> StepsTo x z
}

fun trans : (x, y, z : X) -> StepsTo x y -> StepsTo y z -> StepsTo x z
{ trans x y .y p done          = p
; trans x y  z p (next z' r q) = next z' r (trans x y z' p q)
}

fun const : (x, y : X) -> StepsTo x y -> Eq X (f x) x -> Eq X x y
{ const x .x done q = refl
-- ; const _ _  (next _ r p) q = -- TODO
}

{-
fun bad : (x, y, z : X) -> StepsTo x z -> Eq X (f z) x -> StepsTo x y -> StepsTo y x
{ bad x y z (next z' p q) r s =
  next z r (bad z y _ (trans _ _ _ (next _ r done) q) p (trans _ _ _ (next _ r done) s))
}  -- Giving up, need unification!!

-}
{-
data _=⟨_⟩⇒*_ {X : Set}(x : X)(f : X → X) : X → Set where
  done : x =⟨ f ⟩⇒* x
  next : ∀{y z} → f y ≡ z → x =⟨ f ⟩⇒* y → x =⟨ f ⟩⇒* z

trans* : ∀{X}{x y z}{f : X → X} → x =⟨ f ⟩⇒* y → y =⟨ f ⟩⇒* z → x =⟨ f ⟩⇒* z
trans* p done = p
trans* p (next r q) = next r (trans* p q)

const* : ∀{X}{x y}{f : X → X} → x =⟨ f ⟩⇒* y → f x ≡ x → x ≡ y
const* done q = refl
const* (next r p) q with const* p q
const* (next r p) q | refl = trans (sym q) r

bad : ∀{X}{x y z}{f : X → X} → x =⟨ f ⟩⇒* z → f z ≡ x → x =⟨ f ⟩⇒* y →
      y =⟨ f ⟩⇒* x
bad done p q rewrite const* q p = done
bad (next p q) r s =
  next r (bad (trans* (next r done) q) p (trans* (next r done) s))
-}
