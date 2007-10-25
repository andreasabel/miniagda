-- size no used
fun foo : Size -> Set
{
foo ($ i) = foo i
}

-- const foos : Set = foo #


data Bla : Set 
{
bla : Bla
}



-- size not used
fun foo2 : Size -> Bla -> Set
{
foo2 ($ i) bla = foo2 i bla
}  

-- const foos2 : Set = foo2 # bla


data SBla : Size -> Set
{
sbla : (i : Size ) -> SBla ($ i);
}

