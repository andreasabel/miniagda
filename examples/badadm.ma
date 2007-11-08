-- incomplete pattern
fun foo : Size -> Set
{
foo ($ i) = foo i
}

eval const foos : Set = foo #


data Bla : Set 
{
bla : Bla
}

-- incomplete pattern
fun foo2 : Size -> Bla -> Set
{
foo2 ($ i) bla = foo2 i bla
}  

-- eval const foos2 : Set = foo2 # bla


data SBla : Size -> Set
{
sbla : (i : Size ) -> SBla ($ i);
}

