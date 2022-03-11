module SCTParser where

import qualified Lexer as T
import SCTSyntax

-- parser produced by Happy Version 1.18.2

data HappyAbsSyn
        = HappyTerminal (T.Token)
        | HappyErrorToken Int
        | HappyAbsSyn4 (Defs)
        | HappyAbsSyn5 ([Def])
        | HappyAbsSyn6 (Def)
        | HappyAbsSyn7 (LHS)
        | HappyAbsSyn8 ([Param])
        | HappyAbsSyn9 (RHS)
        | HappyAbsSyn10 (Call)
        | HappyAbsSyn11 ([Arg])
        | HappyAbsSyn12 (Arg)
        | HappyAbsSyn13 (Int)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m =
           Int
        -> (T.Token)
        -> HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> m HappyAbsSyn)
        -> [HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> m HappyAbsSyn)]
        -> HappyStk HappyAbsSyn
        -> [(T.Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
           Int
        -> (T.Token)
        -> HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)
        -> [HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)]
        -> HappyStk HappyAbsSyn
        -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17 :: () => ({-HappyReduction (HappyIdentity) = -}
           Int
        -> (T.Token)
        -> HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)
        -> [HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)]
        -> HappyStk HappyAbsSyn
        -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (14) = happyShift action_6
action_2 (6) = happyGoto action_4
action_2 (7) = happyGoto action_5
action_2 _ = happyReduce_1

action_3 (36) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (30) = happyShift action_8
action_5 _ = happyFail

action_6 (22) = happyShift action_7
action_6 _ = happyFail

action_7 (14) = happyShift action_13
action_7 (8) = happyGoto action_12
action_7 _ = happyFail

action_8 (14) = happyShift action_11
action_8 (9) = happyGoto action_9
action_8 (10) = happyGoto action_10
action_8 _ = happyFail

action_9 (24) = happyShift action_17
action_9 _ = happyReduce_4

action_10 _ = happyReduce_9

action_11 (22) = happyShift action_16
action_11 _ = happyFail

action_12 (23) = happyShift action_14
action_12 (27) = happyShift action_15
action_12 _ = happyFail

action_13 _ = happyReduce_7

action_14 _ = happyReduce_5

action_15 (14) = happyShift action_24
action_15 _ = happyFail

action_16 (14) = happyShift action_22
action_16 (15) = happyShift action_23
action_16 (11) = happyGoto action_19
action_16 (12) = happyGoto action_20
action_16 (13) = happyGoto action_21
action_16 _ = happyFail

action_17 (14) = happyShift action_11
action_17 (10) = happyGoto action_18
action_17 _ = happyFail

action_18 _ = happyReduce_8

action_19 (23) = happyShift action_27
action_19 (27) = happyShift action_28
action_19 _ = happyFail

action_20 _ = happyReduce_12

action_21 _ = happyReduce_13

action_22 (31) = happyShift action_25
action_22 (32) = happyShift action_26
action_22 _ = happyReduce_14

action_23 _ = happyReduce_17

action_24 _ = happyReduce_6

action_25 (15) = happyShift action_23
action_25 (13) = happyGoto action_31
action_25 _ = happyFail

action_26 (15) = happyShift action_23
action_26 (13) = happyGoto action_30
action_26 _ = happyFail

action_27 _ = happyReduce_10

action_28 (14) = happyShift action_22
action_28 (15) = happyShift action_23
action_28 (12) = happyGoto action_29
action_28 (13) = happyGoto action_21
action_28 _ = happyFail

action_29 _ = happyReduce_11

action_30 _ = happyReduce_16

action_31 _ = happyReduce_15

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn4
                 (Defs $ reverse happy_var_1
        )
happyReduction_1 _  = notHappyAtAll

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
                 ([]
        )

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_2 : happy_var_1
        )
happyReduction_3 _ _  = notHappyAtAll

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn6
                 (Def happy_var_1 (reverse happy_var_3)
        )
happyReduction_4 _ _ _  = notHappyAtAll

happyReduce_5 = happyReduce 4 7 happyReduction_5
happyReduction_5 (_ `HappyStk`
        (HappyAbsSyn8  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (T.Id happy_var_1 _)) `HappyStk`
        happyRest)
         = HappyAbsSyn7
                 (LHS happy_var_1 (reverse happy_var_3)
        ) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  8 happyReduction_6
happyReduction_6 (HappyTerminal (T.Id happy_var_3 _))
        _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn8
                 (Param happy_var_3 : happy_var_1
        )
happyReduction_6 _ _ _  = notHappyAtAll

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyTerminal (T.Id happy_var_1 _))
         =  HappyAbsSyn8
                 ([Param happy_var_1]
        )
happyReduction_7 _  = notHappyAtAll

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_3)
        _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_3 : happy_var_1
        )
happyReduction_8 _ _ _  = notHappyAtAll

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_1)
         =  HappyAbsSyn9
                 ([happy_var_1]
        )
happyReduction_9 _  = notHappyAtAll

happyReduce_10 = happyReduce 4 10 happyReduction_10
happyReduction_10 (_ `HappyStk`
        (HappyAbsSyn11  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (T.Id happy_var_1 _)) `HappyStk`
        happyRest)
         = HappyAbsSyn10
                 (Call happy_var_1 (reverse happy_var_3)
        ) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  11 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_3)
        _
        (HappyAbsSyn11  happy_var_1)
         =  HappyAbsSyn11
                 (happy_var_3 : happy_var_1
        )
happyReduction_11 _ _ _  = notHappyAtAll

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_1)
         =  HappyAbsSyn11
                 ([happy_var_1]
        )
happyReduction_12 _  = notHappyAtAll

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 (HappyAbsSyn13  happy_var_1)
         =  HappyAbsSyn12
                 (Const happy_var_1
        )
happyReduction_13 _  = notHappyAtAll

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyTerminal (T.Id happy_var_1 _))
         =  HappyAbsSyn12
                 (Plus happy_var_1 0
        )
happyReduction_14 _  = notHappyAtAll

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_3)
        _
        (HappyTerminal (T.Id happy_var_1 _))
         =  HappyAbsSyn12
                 (Plus happy_var_1 happy_var_3
        )
happyReduction_15 _ _ _  = notHappyAtAll

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_3)
        _
        (HappyTerminal (T.Id happy_var_1 _))
         =  HappyAbsSyn12
                 (Plus happy_var_1 (- happy_var_3)
        )
happyReduction_16 _ _ _  = notHappyAtAll

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyTerminal (T.Number happy_var_1 _))
         =  HappyAbsSyn13
                 (read happy_var_1
        )
happyReduction_17 _  = notHappyAtAll

happyNewToken action sts stk [] =
        action 36 36 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
        let cont i = action i i tk (HappyState action) sts stk tks in
        case tk of {
        T.Id happy_dollar_dollar _ -> cont 14;
        T.Number happy_dollar_dollar _ -> cont 15;
        T.AngleOpen _ -> cont 16;
        T.AngleClose _ -> cont 17;
        T.BrOpen _ -> cont 18;
        T.BrClose _ -> cont 19;
        T.BracketOpen _ -> cont 20;
        T.BracketClose _ -> cont 21;
        T.PrOpen _ -> cont 22;
        T.PrClose _ -> cont 23;
        T.Bar _ -> cont 24;
        T.Sem _ -> cont 25;
        T.Col _ -> cont 26;
        T.Comma _ -> cont 27;
        T.Dot _ -> cont 28;
        T.Arrow _ -> cont 29;
        T.Eq _ -> cont 30;
        T.Plus _ -> cont 31;
        T.Minus _ -> cont 32;
        T.Times _ -> cont 33;
        T.Lam _ -> cont 34;
        T.Underscore _ -> cont 35;
        _ -> happyError' (tk:tks)
        }

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(T.Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [T.Token] -> a
parseError [] = error "Parse error at EOF"
parseError (x : xs) = error ("Parse error at token " ++ T.prettyTok x)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--      trace "failing" $
        happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts))
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
