-- 2010-07-13,-27  state-less stack object

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
                 ([j : Size] -> |j| < |i| -> Stack A j -> A -> Stack A j) ->
                 Stack A i -> A -> Stack A i
{ pushFunc A ($ i) f s a = stack i (just a) s (f i (pushFunc A i f s a))
}
-- f : [j : Size] -> |j| < |$i| -> Stack A j -> A -> Stack A j
-- s : Stack A $i
-- by subtyping
-- f : [j : Size] -> |j| < |i| -> Stack A j -> A -> Stack A j
-- s : Stack A i
-- hence  pushFunc A i f s a : Stack A i
--   f i (...) : A -> Stack A i
-- rhs : Stack A $i

-- tying the knot
cofun pushFix  : [A : Set] -> [i : Size] -> |i| -> Stack A i -> A -> Stack A i
{ pushFix A ($ i) = pushFunc A ($ i) (pushFix A)
}
-- on the rhs, we have the typing of the recursive call
--   pushFix A : [j : Size] -> |j| < |$i| -> Stack A j -> A -> Stack A j

-- constructing the empty stack
cofun empty : [A : Set] -> [i : Size] -> |i| -> Stack A i
{ empty A ($ i) = stack i nothing (empty A i) (pushFix A i (empty A i))
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

{-

  push' s a = fix (\ s' -> Stack (Just a) s (push' s'))

-}
