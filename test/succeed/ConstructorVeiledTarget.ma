-- 2010-09-14
-- 2013-04-05 This should maybe no longer enjoy support, merely obfuscating anyway

let Id ++(A : Set) = A

data Bool : Set
{ true : Bool
; false : Id Bool
}
