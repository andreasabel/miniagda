sized data Nat : Size -> Set
{
  zero : [i : Size] -> Nat ($ i);
  succ : [i : Size] -> Nat i -> Nat ($ i);
}

sized codata CoNat : Size -> Set
{
  cozero : [i : Size] -> CoNat ($ i);
  cosucc : [i : Size] -> CoNat i -> CoNat ($ i)
}

let z : CoNat # = cozero #

-- ok
fun convert2 : [i : Size] -> Nat i -> CoNat i
{
convert2 ($ i) (zero .i) = cozero i;
convert2 ($ i) (succ .i x) = cosucc i (convert2 i x)
}

-- NOT ok
fun convert2' : [i : Size] -> Nat i -> CoNat i
{ convert2' i (zero (i > j))   = cozero j
; convert2' i (succ (i > j) x) = cosucc j (convert2' j x)
}
-- since $j <= i but noth otherwise!

-- ok
fun convert3 : [i : Size] -> Nat i -> CoNat #
{
convert3 i (zero (i > j)) = cozero #;
convert3 i (succ (i > j) x) = omega' #
}

-- also ok
cofun convert4 : [i : Size] -> Nat i -> CoNat i
{
convert4 ($ i) (zero .i) = cozero ($ i) ;
convert4 ($ i) (succ .i x) = cosucc i (convert4 i x)
}

