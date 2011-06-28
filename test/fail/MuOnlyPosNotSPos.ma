-- 2010-06-20

-- F needs to be ++ (spos) not just pos
data Mu ++(F : +Set -> Set) : Set
{ inn : F (Mu F) -> Mu F
}
