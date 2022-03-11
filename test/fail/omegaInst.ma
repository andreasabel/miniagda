-- 2012-02-06  Make sure not to violate < - Constraints by going through infty

let ok  [F : Size -> Set] [i <  #] (f : [j < $i] -> F j) : F i
  = f i

-- this needs to fail, because i can be instantiated to #
let bad [F : Size -> Set] [i <= #] (f : [j < $i] -> F j) : F i
  = f i

let inst [F : Size -> Set] (f : [j < #] -> F j) : F #
  = bad F # f

let bot [F : Size -> Set] (f : [j < #] -> F j) : F #
  = f #
-- DOUBTS: is this so bad after all?
-- each descending chain f has a limit.
-- If # is that closure ordinal, this should be ok.
