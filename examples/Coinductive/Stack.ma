-- 2010-07-13
-- 2013-03-28 use proper bounded quantification j < i

data Maybe (A : Set) : Set
{ nothing : Maybe A
; just : A -> Maybe A
}

-- stack object
sized codata Stack (A : Set) : Size -> Set
{ stack : [i : Size] ->
  (top  : Maybe A) ->
  (pop  : Stack A i) ->
  (push : A -> Stack A i) -> Stack A $i
}

-- functional to construct push action
cofun pushFunc : [A : Set] -> [i : Size] -> |i| ->
                 ([j < i] -> Stack A j -> A -> Stack A j) ->
                 Stack A i -> A -> Stack A i
{ pushFunc A ($ i) f s a = stack i (just a) s (f i (pushFunc A i f s a))
}

-- tying the knot
cofun pushFix  : [A : Set] -> [i : Size] -> |i| -> Stack A i -> A -> Stack A i
{ pushFix A ($ i) = pushFunc A ($ i) (pushFix A)
} -- cofun because of successor pattern

-- constructing the empty stack
cofun empty : [A : Set] -> [i : Size] -> |i| -> Stack A i
{ empty A ($ i) = stack i (nothing) (empty A i) (pushFix A i (empty A i))
}

-- constructing an infinite stack 2012-06-22
cofun repeat : [A : Set] -> (a : A) -> [i : Size] -> |i| -> Stack A i
{ repeat A a ($ i) = stack i (just a) (repeat A a i) (pushFix A i (repeat A a i))
}
{- original circular program

data Stack a = Stack
  { top  :: Maybe a
  , pop  :: Stack a
  , push :: a -> Stack a
  }

-- circular auxiliary program to construct stacks
push' :: Stack a -> a -> Stack a
push' s a = s'
  where s' = Stack (Just a) s (push' s')

-- the empty stack
empty :: Stack a
empty = Stack Nothing empty (push' empty)

-}

-- with measures -----------------------------------------------------
{-
cofun pushFunc : [A : Set] -> [i : Size] -> |i| ->
                 ([j : Size] -> |j| < |i| -> Stack A j -> A -> Stack A j) ->
                 Stack A i -> A -> Stack A i
{ pushFunc A ($ i) f s a = stack A i (just A a) s (f i (pushFunc A i f s a))
}

cofun pushFix  : [A : Set] -> [i : Size] -> |i| -> Stack A i -> A -> Stack A i
{ pushFix A ($ i) |$i|
   -- at this point introduce recursive hypothesis into the context
   -- pushFix : [A:Set] -> [j:Size] -> |j| < |$i| -> Stack A j -> A -> Stack A j
   = pushFunc A ($ i) (pushFix A)
}
-}
   {- in general, if
        f : Delta -> mu -> T
        f ps_Delta mu' ...
      add hypothesis f : Delta -> mu<mu' -> T
      where Delta captures no FVs in mu'
    -}
