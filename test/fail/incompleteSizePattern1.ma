
data Empty : Set
{
}

-- recursion on Size fails since ($ i) is not a complete pattern match
cofun bad' : Size -> Empty
{
  bad' ($ i) = bad' _
}

