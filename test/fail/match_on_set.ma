data A : Set {}
data B : Set {}

fun bla : Set -> Set
{
  bla A = B;
  bla B = A
}
