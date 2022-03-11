
data Bool : Set
{
  tt : Bool;
  ff : Bool
}

fun bad'' : Size -> Bool
{
  bad'' ($ i) = bad'' _;
  bad'' i = tt
}
-- 2010-08-18  ($ i) only allowed in cofun
