-- incomplete pattern match
fun foo : Size -> Set
{
foo (s i) = foo i
}

-- const foos : Set = foo infty


data Bla : Set 
{
bla : Bla
}



-- size not used
fun foo2 : Size -> Bla -> Set
{
foo2 (s i) bla = foo2 i bla
}  

-- const foos2 : Set = foo2 infty bla


data SBla : Size -> Set
{
sbla : (i : Size ) -> SBla (s i);
}

