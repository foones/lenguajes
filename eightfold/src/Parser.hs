{-# OPTIONS_GHC -w #-}
module Parser(Parser, parseProgram, parseQuery) where

import Lexer
import Lang

type Parser = String -> M Program

-- parser produced by Happy Version 1.19.3

data HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

action_0 (15) = happyShift action_15
action_0 (21) = happyShift action_12
action_0 (5) = happyGoto action_13
action_0 (8) = happyGoto action_14
action_0 _ = happyReduce_2

action_1 (13) = happyShift action_8
action_1 (14) = happyShift action_9
action_1 (15) = happyShift action_10
action_1 (16) = happyShift action_11
action_1 (21) = happyShift action_12
action_1 (6) = happyGoto action_3
action_1 (8) = happyGoto action_4
action_1 (9) = happyGoto action_5
action_1 (10) = happyGoto action_6
action_1 (12) = happyGoto action_7
action_1 _ = happyReduce_4

action_2 _ = happyFail

action_3 (1) = happyAccept
action_3 _ = happyFail

action_4 (19) = happyShift action_28
action_4 (7) = happyGoto action_29
action_4 _ = happyReduce_7

action_5 (19) = happyShift action_28
action_5 (7) = happyGoto action_27
action_5 _ = happyReduce_7

action_6 (15) = happyShift action_20
action_6 (16) = happyShift action_11
action_6 (12) = happyGoto action_26
action_6 _ = happyReduce_16

action_7 _ = happyReduce_17

action_8 (15) = happyShift action_25
action_8 (11) = happyGoto action_24
action_8 _ = happyFail

action_9 (15) = happyShift action_20
action_9 (16) = happyShift action_11
action_9 (12) = happyGoto action_23
action_9 _ = happyFail

action_10 (13) = happyShift action_16
action_10 (20) = happyShift action_17
action_10 _ = happyReduce_21

action_11 (13) = happyShift action_8
action_11 (14) = happyShift action_9
action_11 (15) = happyShift action_20
action_11 (16) = happyShift action_11
action_11 (9) = happyGoto action_22
action_11 (10) = happyGoto action_6
action_11 (12) = happyGoto action_7
action_11 _ = happyFail

action_12 (13) = happyShift action_8
action_12 (14) = happyShift action_9
action_12 (15) = happyShift action_20
action_12 (16) = happyShift action_11
action_12 (21) = happyShift action_21
action_12 (9) = happyGoto action_19
action_12 (10) = happyGoto action_6
action_12 (12) = happyGoto action_7
action_12 _ = happyFail

action_13 (22) = happyAccept
action_13 _ = happyFail

action_14 (19) = happyShift action_18
action_14 _ = happyFail

action_15 (13) = happyShift action_16
action_15 (20) = happyShift action_17
action_15 _ = happyFail

action_16 (13) = happyShift action_8
action_16 (14) = happyShift action_9
action_16 (15) = happyShift action_20
action_16 (16) = happyShift action_11
action_16 (9) = happyGoto action_38
action_16 (10) = happyGoto action_6
action_16 (12) = happyGoto action_7
action_16 _ = happyFail

action_17 (13) = happyShift action_8
action_17 (14) = happyShift action_9
action_17 (15) = happyShift action_20
action_17 (16) = happyShift action_11
action_17 (9) = happyGoto action_37
action_17 (10) = happyGoto action_6
action_17 (12) = happyGoto action_7
action_17 _ = happyFail

action_18 (15) = happyShift action_15
action_18 (21) = happyShift action_12
action_18 (5) = happyGoto action_36
action_18 (8) = happyGoto action_14
action_18 _ = happyReduce_2

action_19 _ = happyReduce_12

action_20 _ = happyReduce_21

action_21 (13) = happyShift action_8
action_21 (14) = happyShift action_9
action_21 (15) = happyShift action_20
action_21 (16) = happyShift action_11
action_21 (9) = happyGoto action_35
action_21 (10) = happyGoto action_6
action_21 (12) = happyGoto action_7
action_21 _ = happyFail

action_22 (17) = happyShift action_34
action_22 _ = happyFail

action_23 (13) = happyShift action_8
action_23 (14) = happyShift action_9
action_23 (15) = happyShift action_20
action_23 (16) = happyShift action_11
action_23 (9) = happyGoto action_33
action_23 (10) = happyGoto action_6
action_23 (12) = happyGoto action_7
action_23 _ = happyFail

action_24 (18) = happyShift action_31
action_24 (19) = happyShift action_32
action_24 _ = happyFail

action_25 (15) = happyShift action_20
action_25 (16) = happyShift action_11
action_25 (10) = happyGoto action_30
action_25 (12) = happyGoto action_7
action_25 _ = happyFail

action_26 _ = happyReduce_18

action_27 _ = happyReduce_6

action_28 _ = happyReduce_8

action_29 _ = happyReduce_5

action_30 (15) = happyShift action_20
action_30 (16) = happyShift action_11
action_30 (12) = happyGoto action_26
action_30 _ = happyReduce_19

action_31 (15) = happyShift action_41
action_31 _ = happyFail

action_32 (13) = happyShift action_8
action_32 (14) = happyShift action_9
action_32 (15) = happyShift action_20
action_32 (16) = happyShift action_11
action_32 (9) = happyGoto action_40
action_32 (10) = happyGoto action_6
action_32 (12) = happyGoto action_7
action_32 _ = happyFail

action_33 _ = happyReduce_15

action_34 _ = happyReduce_22

action_35 _ = happyReduce_13

action_36 _ = happyReduce_3

action_37 _ = happyReduce_11

action_38 (20) = happyShift action_39
action_38 _ = happyReduce_9

action_39 (13) = happyShift action_8
action_39 (14) = happyShift action_9
action_39 (15) = happyShift action_20
action_39 (16) = happyShift action_11
action_39 (9) = happyGoto action_43
action_39 (10) = happyGoto action_6
action_39 (12) = happyGoto action_7
action_39 _ = happyFail

action_40 _ = happyReduce_14

action_41 (15) = happyShift action_20
action_41 (16) = happyShift action_11
action_41 (10) = happyGoto action_42
action_41 (12) = happyGoto action_7
action_41 _ = happyFail

action_42 (15) = happyShift action_20
action_42 (16) = happyShift action_11
action_42 (12) = happyGoto action_26
action_42 _ = happyReduce_20

action_43 _ = happyReduce_10

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  6 happyReduction_4
happyReduction_4  =  HappyAbsSyn6
		 ([]
	)

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  6 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 ([AskType happy_var_1]
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  7 happyReduction_7
happyReduction_7  =  HappyAbsSyn7
		 (()
	)

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn7
		 (()
	)

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn8
		 (Assume happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 5 8 happyReduction_10
happyReduction_10 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Prove happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  8 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn8
		 (Define happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  8 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (AskType happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
	_
	_
	 =  HappyAbsSyn8
		 (AskValue happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 9 happyReduction_14
happyReduction_14 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (foldr (uncurry Lam) happy_var_4 happy_var_2
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Lam "_" happy_var_2 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  10 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (App happy_var_1 happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  11 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn11
		 ([(happy_var_1, happy_var_2)]
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 11 happyReduction_20
happyReduction_20 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyTerminal (TokenId happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (happy_var_1 ++ [(happy_var_3, happy_var_4)]
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  12 happyReduction_21
happyReduction_21 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn12
		 (Var happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  12 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 22 22 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenColon -> cont 13;
	TokenFun -> cont 14;
	TokenId happy_dollar_dollar -> cont 15;
	TokenLParen -> cont 16;
	TokenRParen -> cont 17;
	TokenComma -> cont 18;
	TokenDot -> cont 19;
	TokenProof -> cont 20;
	TokenAsk -> cont 21;
	_ -> happyError' (tk:tks)
	}

happyError_ 22 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => M a -> (a -> M b) -> M b
happyThen = (>>=)
happyReturn :: () => a -> M a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> M a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> M a
happyError' = parseError

parseProgramTokens tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

parseQueryTokens tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> M a
parseError msg = fail "parse error"

parseProgram :: Parser
parseProgram = parseProgramTokens . tokenize

parseQuery :: Parser
parseQuery = parseQueryTokens . tokenize
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 5 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

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

notHappyAtAll :: a
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

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
