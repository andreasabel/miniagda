{-# OPTIONS_GHC -w #-}
module Parser where

import qualified Lexer as T
import qualified Concrete as C

import Abstract (Decoration(..),Dec,defaultDec,Override(..))
import Polarity (Pol(..))
import qualified Abstract as A
import qualified Polarity as A
import Concrete (Name)

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (T.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([C.Declaration])
	| HappyAbsSyn6 (C.Declaration)
	| HappyAbsSyn16 ([Name])
	| HappyAbsSyn17 (Name)
	| HappyAbsSyn20 (Pol)
	| HappyAbsSyn21 (A.Measure C.Expr)
	| HappyAbsSyn22 ([C.Expr])
	| HappyAbsSyn23 (A.Bound C.Expr)
	| HappyAbsSyn25 (C.TBind)
	| HappyAbsSyn28 (C.Expr)
	| HappyAbsSyn34 ([([Name],C.Expr)])
	| HappyAbsSyn35 (([Name],C.Expr))
	| HappyAbsSyn36 (C.TypeSig)
	| HappyAbsSyn37 (C.Constructor)
	| HappyAbsSyn38 ([C.Constructor ])
	| HappyAbsSyn39 ([C.Clause])
	| HappyAbsSyn40 (C.Clause)
	| HappyAbsSyn41 ([C.Pattern])
	| HappyAbsSyn43 (C.Pattern)
	| HappyAbsSyn46 ((Name, [C.Pattern]))
	| HappyAbsSyn48 ([C.Clause ])
	| HappyAbsSyn50 (C.Telescope)

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
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300,
 action_301,
 action_302,
 action_303,
 action_304,
 action_305,
 action_306,
 action_307,
 action_308,
 action_309,
 action_310,
 action_311,
 action_312,
 action_313,
 action_314,
 action_315,
 action_316,
 action_317,
 action_318,
 action_319,
 action_320,
 action_321 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141 :: () => ({-HappyReduction (HappyIdentity) = -}
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

action_2 (53) = happyShift action_14
action_2 (54) = happyShift action_15
action_2 (55) = happyShift action_16
action_2 (56) = happyShift action_17
action_2 (58) = happyShift action_18
action_2 (59) = happyShift action_19
action_2 (60) = happyShift action_20
action_2 (63) = happyShift action_21
action_2 (65) = happyShift action_22
action_2 (66) = happyShift action_23
action_2 (67) = happyShift action_24
action_2 (68) = happyShift action_25
action_2 (69) = happyShift action_26
action_2 (6) = happyGoto action_4
action_2 (7) = happyGoto action_5
action_2 (8) = happyGoto action_6
action_2 (9) = happyGoto action_7
action_2 (10) = happyGoto action_8
action_2 (11) = happyGoto action_9
action_2 (12) = happyGoto action_10
action_2 (13) = happyGoto action_11
action_2 (14) = happyGoto action_12
action_2 (15) = happyGoto action_13
action_2 _ = happyReduce_1

action_3 (102) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 _ = happyReduce_4

action_6 _ = happyReduce_6

action_7 _ = happyReduce_5

action_8 _ = happyReduce_7

action_9 _ = happyReduce_8

action_10 _ = happyReduce_9

action_11 _ = happyReduce_10

action_12 _ = happyReduce_11

action_13 _ = happyReduce_12

action_14 (51) = happyShift action_38
action_14 (17) = happyGoto action_46
action_14 _ = happyFail

action_15 (51) = happyShift action_38
action_15 (17) = happyGoto action_45
action_15 _ = happyFail

action_16 (51) = happyShift action_38
action_16 (17) = happyGoto action_44
action_16 _ = happyFail

action_17 (53) = happyShift action_42
action_17 (54) = happyShift action_43
action_17 _ = happyFail

action_18 (79) = happyShift action_41
action_18 _ = happyFail

action_19 (51) = happyShift action_38
action_19 (17) = happyGoto action_36
action_19 (36) = happyGoto action_40
action_19 _ = happyFail

action_20 (51) = happyShift action_38
action_20 (17) = happyGoto action_36
action_20 (36) = happyGoto action_39
action_20 _ = happyFail

action_21 (51) = happyShift action_38
action_21 (17) = happyGoto action_36
action_21 (36) = happyGoto action_37
action_21 _ = happyFail

action_22 (63) = happyShift action_35
action_22 _ = happyFail

action_23 (53) = happyShift action_14
action_23 (54) = happyShift action_15
action_23 (55) = happyShift action_16
action_23 (56) = happyShift action_17
action_23 (58) = happyShift action_18
action_23 (59) = happyShift action_19
action_23 (60) = happyShift action_20
action_23 (63) = happyShift action_21
action_23 (65) = happyShift action_22
action_23 (66) = happyShift action_23
action_23 (67) = happyShift action_24
action_23 (68) = happyShift action_25
action_23 (69) = happyShift action_26
action_23 (79) = happyShift action_34
action_23 (6) = happyGoto action_33
action_23 (7) = happyGoto action_5
action_23 (8) = happyGoto action_6
action_23 (9) = happyGoto action_7
action_23 (10) = happyGoto action_8
action_23 (11) = happyGoto action_9
action_23 (12) = happyGoto action_10
action_23 (13) = happyGoto action_11
action_23 (14) = happyGoto action_12
action_23 (15) = happyGoto action_13
action_23 _ = happyFail

action_24 (53) = happyShift action_14
action_24 (54) = happyShift action_15
action_24 (55) = happyShift action_16
action_24 (56) = happyShift action_17
action_24 (58) = happyShift action_18
action_24 (59) = happyShift action_19
action_24 (60) = happyShift action_20
action_24 (63) = happyShift action_21
action_24 (65) = happyShift action_22
action_24 (66) = happyShift action_23
action_24 (67) = happyShift action_24
action_24 (68) = happyShift action_25
action_24 (69) = happyShift action_26
action_24 (79) = happyShift action_32
action_24 (6) = happyGoto action_31
action_24 (7) = happyGoto action_5
action_24 (8) = happyGoto action_6
action_24 (9) = happyGoto action_7
action_24 (10) = happyGoto action_8
action_24 (11) = happyGoto action_9
action_24 (12) = happyGoto action_10
action_24 (13) = happyGoto action_11
action_24 (14) = happyGoto action_12
action_24 (15) = happyGoto action_13
action_24 _ = happyFail

action_25 (53) = happyShift action_14
action_25 (54) = happyShift action_15
action_25 (55) = happyShift action_16
action_25 (56) = happyShift action_17
action_25 (58) = happyShift action_18
action_25 (59) = happyShift action_19
action_25 (60) = happyShift action_20
action_25 (63) = happyShift action_21
action_25 (65) = happyShift action_22
action_25 (66) = happyShift action_23
action_25 (67) = happyShift action_24
action_25 (68) = happyShift action_25
action_25 (69) = happyShift action_26
action_25 (79) = happyShift action_30
action_25 (6) = happyGoto action_29
action_25 (7) = happyGoto action_5
action_25 (8) = happyGoto action_6
action_25 (9) = happyGoto action_7
action_25 (10) = happyGoto action_8
action_25 (11) = happyGoto action_9
action_25 (12) = happyGoto action_10
action_25 (13) = happyGoto action_11
action_25 (14) = happyGoto action_12
action_25 (15) = happyGoto action_13
action_25 _ = happyFail

action_26 (53) = happyShift action_14
action_26 (54) = happyShift action_15
action_26 (55) = happyShift action_16
action_26 (56) = happyShift action_17
action_26 (58) = happyShift action_18
action_26 (59) = happyShift action_19
action_26 (60) = happyShift action_20
action_26 (63) = happyShift action_21
action_26 (65) = happyShift action_22
action_26 (66) = happyShift action_23
action_26 (67) = happyShift action_24
action_26 (68) = happyShift action_25
action_26 (69) = happyShift action_26
action_26 (79) = happyShift action_28
action_26 (6) = happyGoto action_27
action_26 (7) = happyGoto action_5
action_26 (8) = happyGoto action_6
action_26 (9) = happyGoto action_7
action_26 (10) = happyGoto action_8
action_26 (11) = happyGoto action_9
action_26 (12) = happyGoto action_10
action_26 (13) = happyGoto action_11
action_26 (14) = happyGoto action_12
action_26 (15) = happyGoto action_13
action_26 _ = happyFail

action_27 _ = happyReduce_13

action_28 (5) = happyGoto action_71
action_28 _ = happyReduce_2

action_29 _ = happyReduce_19

action_30 (5) = happyGoto action_70
action_30 _ = happyReduce_2

action_31 _ = happyReduce_17

action_32 (5) = happyGoto action_69
action_32 _ = happyReduce_2

action_33 _ = happyReduce_15

action_34 (5) = happyGoto action_68
action_34 _ = happyReduce_2

action_35 (51) = happyShift action_38
action_35 (17) = happyGoto action_36
action_35 (36) = happyGoto action_67
action_35 _ = happyFail

action_36 (88) = happyShift action_66
action_36 _ = happyFail

action_37 (92) = happyShift action_65
action_37 _ = happyFail

action_38 _ = happyReduce_33

action_39 (79) = happyShift action_64
action_39 _ = happyFail

action_40 (79) = happyShift action_63
action_40 _ = happyFail

action_41 (5) = happyGoto action_62
action_41 _ = happyReduce_2

action_42 (51) = happyShift action_38
action_42 (17) = happyGoto action_61
action_42 _ = happyFail

action_43 (51) = happyShift action_38
action_43 (17) = happyGoto action_60
action_43 _ = happyFail

action_44 (81) = happyShift action_50
action_44 (83) = happyShift action_51
action_44 (89) = happyShift action_52
action_44 (93) = happyShift action_53
action_44 (94) = happyShift action_54
action_44 (95) = happyShift action_55
action_44 (97) = happyShift action_56
action_44 (98) = happyShift action_57
action_44 (20) = happyGoto action_47
action_44 (49) = happyGoto action_48
action_44 (50) = happyGoto action_59
action_44 _ = happyReduce_140

action_45 (81) = happyShift action_50
action_45 (83) = happyShift action_51
action_45 (89) = happyShift action_52
action_45 (93) = happyShift action_53
action_45 (94) = happyShift action_54
action_45 (95) = happyShift action_55
action_45 (97) = happyShift action_56
action_45 (98) = happyShift action_57
action_45 (20) = happyGoto action_47
action_45 (49) = happyGoto action_48
action_45 (50) = happyGoto action_58
action_45 _ = happyReduce_140

action_46 (81) = happyShift action_50
action_46 (83) = happyShift action_51
action_46 (89) = happyShift action_52
action_46 (93) = happyShift action_53
action_46 (94) = happyShift action_54
action_46 (95) = happyShift action_55
action_46 (97) = happyShift action_56
action_46 (98) = happyShift action_57
action_46 (20) = happyGoto action_47
action_46 (49) = happyGoto action_48
action_46 (50) = happyGoto action_49
action_46 _ = happyReduce_140

action_47 (83) = happyShift action_123
action_47 _ = happyFail

action_48 (81) = happyShift action_50
action_48 (83) = happyShift action_51
action_48 (89) = happyShift action_52
action_48 (93) = happyShift action_53
action_48 (94) = happyShift action_54
action_48 (95) = happyShift action_55
action_48 (97) = happyShift action_56
action_48 (98) = happyShift action_57
action_48 (20) = happyGoto action_47
action_48 (49) = happyGoto action_48
action_48 (50) = happyGoto action_122
action_48 _ = happyReduce_140

action_49 (88) = happyShift action_121
action_49 _ = happyFail

action_50 (51) = happyShift action_38
action_50 (17) = happyGoto action_116
action_50 (19) = happyGoto action_120
action_50 _ = happyFail

action_51 (51) = happyShift action_38
action_51 (56) = happyShift action_118
action_51 (94) = happyShift action_119
action_51 (17) = happyGoto action_116
action_51 (19) = happyGoto action_117
action_51 _ = happyFail

action_52 _ = happyReduce_41

action_53 _ = happyReduce_38

action_54 _ = happyReduce_39

action_55 _ = happyReduce_40

action_56 _ = happyReduce_43

action_57 _ = happyReduce_42

action_58 (88) = happyShift action_115
action_58 _ = happyFail

action_59 (88) = happyShift action_114
action_59 _ = happyFail

action_60 (81) = happyShift action_50
action_60 (83) = happyShift action_51
action_60 (89) = happyShift action_52
action_60 (93) = happyShift action_53
action_60 (94) = happyShift action_54
action_60 (95) = happyShift action_55
action_60 (97) = happyShift action_56
action_60 (98) = happyShift action_57
action_60 (20) = happyGoto action_47
action_60 (49) = happyGoto action_48
action_60 (50) = happyGoto action_113
action_60 _ = happyReduce_140

action_61 (81) = happyShift action_50
action_61 (83) = happyShift action_51
action_61 (89) = happyShift action_52
action_61 (93) = happyShift action_53
action_61 (94) = happyShift action_54
action_61 (95) = happyShift action_55
action_61 (97) = happyShift action_56
action_61 (98) = happyShift action_57
action_61 (20) = happyGoto action_47
action_61 (49) = happyGoto action_48
action_61 (50) = happyGoto action_112
action_61 _ = happyReduce_140

action_62 (53) = happyShift action_14
action_62 (54) = happyShift action_15
action_62 (55) = happyShift action_16
action_62 (56) = happyShift action_17
action_62 (58) = happyShift action_18
action_62 (59) = happyShift action_19
action_62 (60) = happyShift action_20
action_62 (63) = happyShift action_21
action_62 (65) = happyShift action_22
action_62 (66) = happyShift action_23
action_62 (67) = happyShift action_24
action_62 (68) = happyShift action_25
action_62 (69) = happyShift action_26
action_62 (80) = happyShift action_111
action_62 (6) = happyGoto action_4
action_62 (7) = happyGoto action_5
action_62 (8) = happyGoto action_6
action_62 (9) = happyGoto action_7
action_62 (10) = happyGoto action_8
action_62 (11) = happyGoto action_9
action_62 (12) = happyGoto action_10
action_62 (13) = happyGoto action_11
action_62 (14) = happyGoto action_12
action_62 (15) = happyGoto action_13
action_62 _ = happyFail

action_63 (51) = happyShift action_38
action_63 (17) = happyGoto action_106
action_63 (40) = happyGoto action_107
action_63 (47) = happyGoto action_110
action_63 (48) = happyGoto action_109
action_63 _ = happyReduce_134

action_64 (51) = happyShift action_38
action_64 (17) = happyGoto action_106
action_64 (40) = happyGoto action_107
action_64 (47) = happyGoto action_108
action_64 (48) = happyGoto action_109
action_64 _ = happyReduce_134

action_65 (51) = happyShift action_38
action_65 (52) = happyShift action_87
action_65 (55) = happyShift action_88
action_65 (61) = happyShift action_89
action_65 (63) = happyShift action_90
action_65 (71) = happyShift action_91
action_65 (72) = happyShift action_92
action_65 (73) = happyShift action_93
action_65 (74) = happyShift action_94
action_65 (75) = happyShift action_95
action_65 (76) = happyShift action_96
action_65 (77) = happyShift action_97
action_65 (81) = happyShift action_98
action_65 (83) = happyShift action_99
action_65 (85) = happyShift action_100
action_65 (89) = happyShift action_52
action_65 (93) = happyShift action_53
action_65 (94) = happyShift action_54
action_65 (95) = happyShift action_55
action_65 (97) = happyShift action_56
action_65 (98) = happyShift action_57
action_65 (100) = happyShift action_101
action_65 (101) = happyShift action_102
action_65 (17) = happyGoto action_77
action_65 (20) = happyGoto action_78
action_65 (21) = happyGoto action_79
action_65 (23) = happyGoto action_80
action_65 (25) = happyGoto action_81
action_65 (27) = happyGoto action_82
action_65 (28) = happyGoto action_103
action_65 (29) = happyGoto action_104
action_65 (30) = happyGoto action_105
action_65 (31) = happyGoto action_84
action_65 (32) = happyGoto action_85
action_65 (33) = happyGoto action_86
action_65 _ = happyFail

action_66 (51) = happyShift action_38
action_66 (52) = happyShift action_87
action_66 (55) = happyShift action_88
action_66 (61) = happyShift action_89
action_66 (63) = happyShift action_90
action_66 (71) = happyShift action_91
action_66 (72) = happyShift action_92
action_66 (73) = happyShift action_93
action_66 (74) = happyShift action_94
action_66 (75) = happyShift action_95
action_66 (76) = happyShift action_96
action_66 (77) = happyShift action_97
action_66 (81) = happyShift action_98
action_66 (83) = happyShift action_99
action_66 (85) = happyShift action_100
action_66 (89) = happyShift action_52
action_66 (93) = happyShift action_53
action_66 (94) = happyShift action_54
action_66 (95) = happyShift action_55
action_66 (97) = happyShift action_56
action_66 (98) = happyShift action_57
action_66 (100) = happyShift action_101
action_66 (101) = happyShift action_102
action_66 (17) = happyGoto action_77
action_66 (20) = happyGoto action_78
action_66 (21) = happyGoto action_79
action_66 (23) = happyGoto action_80
action_66 (25) = happyGoto action_81
action_66 (27) = happyGoto action_82
action_66 (30) = happyGoto action_83
action_66 (31) = happyGoto action_84
action_66 (32) = happyGoto action_85
action_66 (33) = happyGoto action_86
action_66 _ = happyFail

action_67 (92) = happyShift action_76
action_67 _ = happyFail

action_68 (53) = happyShift action_14
action_68 (54) = happyShift action_15
action_68 (55) = happyShift action_16
action_68 (56) = happyShift action_17
action_68 (58) = happyShift action_18
action_68 (59) = happyShift action_19
action_68 (60) = happyShift action_20
action_68 (63) = happyShift action_21
action_68 (65) = happyShift action_22
action_68 (66) = happyShift action_23
action_68 (67) = happyShift action_24
action_68 (68) = happyShift action_25
action_68 (69) = happyShift action_26
action_68 (80) = happyShift action_75
action_68 (6) = happyGoto action_4
action_68 (7) = happyGoto action_5
action_68 (8) = happyGoto action_6
action_68 (9) = happyGoto action_7
action_68 (10) = happyGoto action_8
action_68 (11) = happyGoto action_9
action_68 (12) = happyGoto action_10
action_68 (13) = happyGoto action_11
action_68 (14) = happyGoto action_12
action_68 (15) = happyGoto action_13
action_68 _ = happyFail

action_69 (53) = happyShift action_14
action_69 (54) = happyShift action_15
action_69 (55) = happyShift action_16
action_69 (56) = happyShift action_17
action_69 (58) = happyShift action_18
action_69 (59) = happyShift action_19
action_69 (60) = happyShift action_20
action_69 (63) = happyShift action_21
action_69 (65) = happyShift action_22
action_69 (66) = happyShift action_23
action_69 (67) = happyShift action_24
action_69 (68) = happyShift action_25
action_69 (69) = happyShift action_26
action_69 (80) = happyShift action_74
action_69 (6) = happyGoto action_4
action_69 (7) = happyGoto action_5
action_69 (8) = happyGoto action_6
action_69 (9) = happyGoto action_7
action_69 (10) = happyGoto action_8
action_69 (11) = happyGoto action_9
action_69 (12) = happyGoto action_10
action_69 (13) = happyGoto action_11
action_69 (14) = happyGoto action_12
action_69 (15) = happyGoto action_13
action_69 _ = happyFail

action_70 (53) = happyShift action_14
action_70 (54) = happyShift action_15
action_70 (55) = happyShift action_16
action_70 (56) = happyShift action_17
action_70 (58) = happyShift action_18
action_70 (59) = happyShift action_19
action_70 (60) = happyShift action_20
action_70 (63) = happyShift action_21
action_70 (65) = happyShift action_22
action_70 (66) = happyShift action_23
action_70 (67) = happyShift action_24
action_70 (68) = happyShift action_25
action_70 (69) = happyShift action_26
action_70 (80) = happyShift action_73
action_70 (6) = happyGoto action_4
action_70 (7) = happyGoto action_5
action_70 (8) = happyGoto action_6
action_70 (9) = happyGoto action_7
action_70 (10) = happyGoto action_8
action_70 (11) = happyGoto action_9
action_70 (12) = happyGoto action_10
action_70 (13) = happyGoto action_11
action_70 (14) = happyGoto action_12
action_70 (15) = happyGoto action_13
action_70 _ = happyFail

action_71 (53) = happyShift action_14
action_71 (54) = happyShift action_15
action_71 (55) = happyShift action_16
action_71 (56) = happyShift action_17
action_71 (58) = happyShift action_18
action_71 (59) = happyShift action_19
action_71 (60) = happyShift action_20
action_71 (63) = happyShift action_21
action_71 (65) = happyShift action_22
action_71 (66) = happyShift action_23
action_71 (67) = happyShift action_24
action_71 (68) = happyShift action_25
action_71 (69) = happyShift action_26
action_71 (80) = happyShift action_72
action_71 (6) = happyGoto action_4
action_71 (7) = happyGoto action_5
action_71 (8) = happyGoto action_6
action_71 (9) = happyGoto action_7
action_71 (10) = happyGoto action_8
action_71 (11) = happyGoto action_9
action_71 (12) = happyGoto action_10
action_71 (13) = happyGoto action_11
action_71 (14) = happyGoto action_12
action_71 (15) = happyGoto action_13
action_71 _ = happyFail

action_72 _ = happyReduce_14

action_73 _ = happyReduce_20

action_74 _ = happyReduce_18

action_75 _ = happyReduce_16

action_76 (51) = happyShift action_38
action_76 (52) = happyShift action_87
action_76 (55) = happyShift action_88
action_76 (61) = happyShift action_89
action_76 (63) = happyShift action_90
action_76 (71) = happyShift action_91
action_76 (72) = happyShift action_92
action_76 (73) = happyShift action_93
action_76 (74) = happyShift action_94
action_76 (75) = happyShift action_95
action_76 (76) = happyShift action_96
action_76 (77) = happyShift action_97
action_76 (81) = happyShift action_98
action_76 (83) = happyShift action_99
action_76 (85) = happyShift action_100
action_76 (89) = happyShift action_52
action_76 (93) = happyShift action_53
action_76 (94) = happyShift action_54
action_76 (95) = happyShift action_55
action_76 (97) = happyShift action_56
action_76 (98) = happyShift action_57
action_76 (100) = happyShift action_101
action_76 (101) = happyShift action_102
action_76 (17) = happyGoto action_77
action_76 (20) = happyGoto action_78
action_76 (21) = happyGoto action_79
action_76 (23) = happyGoto action_80
action_76 (25) = happyGoto action_81
action_76 (27) = happyGoto action_82
action_76 (28) = happyGoto action_177
action_76 (29) = happyGoto action_104
action_76 (30) = happyGoto action_105
action_76 (31) = happyGoto action_84
action_76 (32) = happyGoto action_85
action_76 (33) = happyGoto action_86
action_76 _ = happyFail

action_77 _ = happyReduce_89

action_78 (51) = happyShift action_38
action_78 (52) = happyShift action_87
action_78 (55) = happyShift action_88
action_78 (71) = happyShift action_91
action_78 (72) = happyShift action_92
action_78 (73) = happyShift action_93
action_78 (74) = happyShift action_94
action_78 (75) = happyShift action_95
action_78 (76) = happyShift action_96
action_78 (77) = happyShift action_97
action_78 (81) = happyShift action_98
action_78 (83) = happyShift action_176
action_78 (85) = happyShift action_100
action_78 (89) = happyShift action_52
action_78 (93) = happyShift action_53
action_78 (94) = happyShift action_54
action_78 (95) = happyShift action_55
action_78 (97) = happyShift action_56
action_78 (98) = happyShift action_57
action_78 (101) = happyShift action_102
action_78 (17) = happyGoto action_77
action_78 (20) = happyGoto action_78
action_78 (21) = happyGoto action_79
action_78 (23) = happyGoto action_80
action_78 (25) = happyGoto action_81
action_78 (27) = happyGoto action_174
action_78 (31) = happyGoto action_175
action_78 (32) = happyGoto action_85
action_78 (33) = happyGoto action_86
action_78 _ = happyFail

action_79 (77) = happyShift action_172
action_79 (91) = happyShift action_173
action_79 _ = happyReduce_65

action_80 _ = happyReduce_66

action_81 _ = happyReduce_64

action_82 (90) = happyShift action_170
action_82 (99) = happyShift action_171
action_82 _ = happyFail

action_83 _ = happyReduce_100

action_84 (90) = happyReduce_61
action_84 (94) = happyShift action_169
action_84 (99) = happyReduce_61
action_84 _ = happyReduce_74

action_85 (51) = happyShift action_38
action_85 (52) = happyShift action_154
action_85 (55) = happyShift action_88
action_85 (71) = happyShift action_167
action_85 (73) = happyShift action_93
action_85 (74) = happyShift action_94
action_85 (75) = happyShift action_95
action_85 (76) = happyShift action_96
action_85 (77) = happyShift action_97
action_85 (83) = happyShift action_155
action_85 (89) = happyShift action_168
action_85 (101) = happyShift action_102
action_85 (17) = happyGoto action_77
action_85 (33) = happyGoto action_166
action_85 _ = happyReduce_76

action_86 _ = happyReduce_82

action_87 (97) = happyShift action_165
action_87 _ = happyReduce_94

action_88 (79) = happyShift action_164
action_88 _ = happyFail

action_89 (51) = happyShift action_38
action_89 (52) = happyShift action_87
action_89 (55) = happyShift action_88
action_89 (61) = happyShift action_89
action_89 (63) = happyShift action_90
action_89 (71) = happyShift action_91
action_89 (72) = happyShift action_92
action_89 (73) = happyShift action_93
action_89 (74) = happyShift action_94
action_89 (75) = happyShift action_95
action_89 (76) = happyShift action_96
action_89 (77) = happyShift action_97
action_89 (81) = happyShift action_98
action_89 (83) = happyShift action_99
action_89 (85) = happyShift action_100
action_89 (89) = happyShift action_52
action_89 (93) = happyShift action_53
action_89 (94) = happyShift action_54
action_89 (95) = happyShift action_55
action_89 (97) = happyShift action_56
action_89 (98) = happyShift action_57
action_89 (100) = happyShift action_101
action_89 (101) = happyShift action_102
action_89 (17) = happyGoto action_77
action_89 (20) = happyGoto action_78
action_89 (21) = happyGoto action_79
action_89 (23) = happyGoto action_80
action_89 (25) = happyGoto action_81
action_89 (27) = happyGoto action_82
action_89 (28) = happyGoto action_163
action_89 (29) = happyGoto action_104
action_89 (30) = happyGoto action_105
action_89 (31) = happyGoto action_84
action_89 (32) = happyGoto action_85
action_89 (33) = happyGoto action_86
action_89 _ = happyFail

action_90 (51) = happyShift action_38
action_90 (81) = happyShift action_161
action_90 (83) = happyShift action_162
action_90 (89) = happyShift action_52
action_90 (93) = happyShift action_53
action_90 (94) = happyShift action_54
action_90 (95) = happyShift action_55
action_90 (97) = happyShift action_56
action_90 (98) = happyShift action_57
action_90 (17) = happyGoto action_158
action_90 (20) = happyGoto action_159
action_90 (26) = happyGoto action_160
action_90 _ = happyFail

action_91 (51) = happyShift action_38
action_91 (52) = happyShift action_154
action_91 (55) = happyShift action_88
action_91 (73) = happyShift action_93
action_91 (74) = happyShift action_94
action_91 (75) = happyShift action_95
action_91 (76) = happyShift action_96
action_91 (77) = happyShift action_97
action_91 (83) = happyShift action_155
action_91 (101) = happyShift action_102
action_91 (17) = happyGoto action_77
action_91 (33) = happyGoto action_157
action_91 _ = happyReduce_78

action_92 (51) = happyShift action_38
action_92 (52) = happyShift action_154
action_92 (55) = happyShift action_88
action_92 (73) = happyShift action_93
action_92 (74) = happyShift action_94
action_92 (75) = happyShift action_95
action_92 (76) = happyShift action_96
action_92 (77) = happyShift action_97
action_92 (83) = happyShift action_155
action_92 (101) = happyShift action_102
action_92 (17) = happyGoto action_77
action_92 (33) = happyGoto action_156
action_92 _ = happyFail

action_93 _ = happyReduce_86

action_94 _ = happyReduce_88

action_95 (51) = happyShift action_38
action_95 (52) = happyShift action_154
action_95 (55) = happyShift action_88
action_95 (73) = happyShift action_93
action_95 (74) = happyShift action_94
action_95 (75) = happyShift action_95
action_95 (76) = happyShift action_96
action_95 (77) = happyShift action_97
action_95 (83) = happyShift action_155
action_95 (101) = happyShift action_102
action_95 (17) = happyGoto action_77
action_95 (33) = happyGoto action_153
action_95 _ = happyFail

action_96 _ = happyReduce_87

action_97 (51) = happyShift action_38
action_97 (52) = happyShift action_87
action_97 (55) = happyShift action_88
action_97 (61) = happyShift action_89
action_97 (63) = happyShift action_90
action_97 (71) = happyShift action_91
action_97 (72) = happyShift action_92
action_97 (73) = happyShift action_93
action_97 (74) = happyShift action_94
action_97 (75) = happyShift action_95
action_97 (76) = happyShift action_96
action_97 (77) = happyShift action_97
action_97 (81) = happyShift action_98
action_97 (83) = happyShift action_99
action_97 (85) = happyShift action_100
action_97 (89) = happyShift action_52
action_97 (93) = happyShift action_53
action_97 (94) = happyShift action_54
action_97 (95) = happyShift action_55
action_97 (97) = happyShift action_56
action_97 (98) = happyShift action_57
action_97 (100) = happyShift action_101
action_97 (101) = happyShift action_102
action_97 (17) = happyGoto action_77
action_97 (20) = happyGoto action_78
action_97 (21) = happyGoto action_79
action_97 (23) = happyGoto action_80
action_97 (25) = happyGoto action_81
action_97 (27) = happyGoto action_82
action_97 (28) = happyGoto action_152
action_97 (29) = happyGoto action_104
action_97 (30) = happyGoto action_105
action_97 (31) = happyGoto action_84
action_97 (32) = happyGoto action_85
action_97 (33) = happyGoto action_86
action_97 _ = happyFail

action_98 (51) = happyShift action_38
action_98 (52) = happyShift action_87
action_98 (55) = happyShift action_88
action_98 (61) = happyShift action_89
action_98 (63) = happyShift action_90
action_98 (71) = happyShift action_91
action_98 (72) = happyShift action_92
action_98 (73) = happyShift action_93
action_98 (74) = happyShift action_94
action_98 (75) = happyShift action_95
action_98 (76) = happyShift action_96
action_98 (77) = happyShift action_97
action_98 (81) = happyShift action_98
action_98 (83) = happyShift action_99
action_98 (85) = happyShift action_100
action_98 (89) = happyShift action_52
action_98 (93) = happyShift action_53
action_98 (94) = happyShift action_54
action_98 (95) = happyShift action_55
action_98 (97) = happyShift action_56
action_98 (98) = happyShift action_57
action_98 (100) = happyShift action_101
action_98 (101) = happyShift action_102
action_98 (17) = happyGoto action_149
action_98 (19) = happyGoto action_150
action_98 (20) = happyGoto action_78
action_98 (21) = happyGoto action_79
action_98 (23) = happyGoto action_80
action_98 (25) = happyGoto action_81
action_98 (27) = happyGoto action_82
action_98 (30) = happyGoto action_151
action_98 (31) = happyGoto action_84
action_98 (32) = happyGoto action_85
action_98 (33) = happyGoto action_86
action_98 _ = happyFail

action_99 (51) = happyShift action_38
action_99 (52) = happyShift action_87
action_99 (55) = happyShift action_88
action_99 (61) = happyShift action_89
action_99 (63) = happyShift action_90
action_99 (71) = happyShift action_91
action_99 (72) = happyShift action_92
action_99 (73) = happyShift action_93
action_99 (74) = happyShift action_94
action_99 (75) = happyShift action_95
action_99 (76) = happyShift action_96
action_99 (77) = happyShift action_97
action_99 (81) = happyShift action_98
action_99 (83) = happyShift action_99
action_99 (85) = happyShift action_100
action_99 (89) = happyShift action_52
action_99 (93) = happyShift action_53
action_99 (94) = happyShift action_54
action_99 (95) = happyShift action_55
action_99 (97) = happyShift action_56
action_99 (98) = happyShift action_57
action_99 (100) = happyShift action_101
action_99 (101) = happyShift action_102
action_99 (17) = happyGoto action_145
action_99 (20) = happyGoto action_78
action_99 (21) = happyGoto action_79
action_99 (23) = happyGoto action_80
action_99 (24) = happyGoto action_146
action_99 (25) = happyGoto action_81
action_99 (27) = happyGoto action_82
action_99 (28) = happyGoto action_147
action_99 (29) = happyGoto action_148
action_99 (30) = happyGoto action_105
action_99 (31) = happyGoto action_84
action_99 (32) = happyGoto action_85
action_99 (33) = happyGoto action_86
action_99 _ = happyFail

action_100 (51) = happyShift action_38
action_100 (52) = happyShift action_87
action_100 (55) = happyShift action_88
action_100 (61) = happyShift action_89
action_100 (63) = happyShift action_90
action_100 (71) = happyShift action_91
action_100 (72) = happyShift action_92
action_100 (73) = happyShift action_93
action_100 (74) = happyShift action_94
action_100 (75) = happyShift action_95
action_100 (76) = happyShift action_96
action_100 (77) = happyShift action_97
action_100 (81) = happyShift action_98
action_100 (83) = happyShift action_99
action_100 (85) = happyShift action_100
action_100 (89) = happyShift action_52
action_100 (93) = happyShift action_53
action_100 (94) = happyShift action_54
action_100 (95) = happyShift action_55
action_100 (97) = happyShift action_56
action_100 (98) = happyShift action_57
action_100 (100) = happyShift action_101
action_100 (101) = happyShift action_102
action_100 (17) = happyGoto action_77
action_100 (20) = happyGoto action_78
action_100 (21) = happyGoto action_79
action_100 (22) = happyGoto action_143
action_100 (23) = happyGoto action_80
action_100 (25) = happyGoto action_81
action_100 (27) = happyGoto action_82
action_100 (30) = happyGoto action_144
action_100 (31) = happyGoto action_84
action_100 (32) = happyGoto action_85
action_100 (33) = happyGoto action_86
action_100 _ = happyFail

action_101 (51) = happyShift action_38
action_101 (17) = happyGoto action_141
action_101 (18) = happyGoto action_142
action_101 _ = happyFail

action_102 _ = happyReduce_92

action_103 _ = happyReduce_29

action_104 _ = happyReduce_67

action_105 (86) = happyShift action_140
action_105 _ = happyReduce_68

action_106 (41) = happyGoto action_138
action_106 (42) = happyGoto action_139
action_106 _ = happyReduce_114

action_107 _ = happyReduce_133

action_108 (80) = happyShift action_137
action_108 _ = happyFail

action_109 (87) = happyShift action_136
action_109 _ = happyReduce_130

action_110 (80) = happyShift action_135
action_110 _ = happyFail

action_111 _ = happyReduce_28

action_112 (88) = happyShift action_134
action_112 _ = happyFail

action_113 (88) = happyShift action_133
action_113 _ = happyFail

action_114 (51) = happyShift action_38
action_114 (52) = happyShift action_87
action_114 (55) = happyShift action_88
action_114 (61) = happyShift action_89
action_114 (63) = happyShift action_90
action_114 (71) = happyShift action_91
action_114 (72) = happyShift action_92
action_114 (73) = happyShift action_93
action_114 (74) = happyShift action_94
action_114 (75) = happyShift action_95
action_114 (76) = happyShift action_96
action_114 (77) = happyShift action_97
action_114 (81) = happyShift action_98
action_114 (83) = happyShift action_99
action_114 (85) = happyShift action_100
action_114 (89) = happyShift action_52
action_114 (93) = happyShift action_53
action_114 (94) = happyShift action_54
action_114 (95) = happyShift action_55
action_114 (97) = happyShift action_56
action_114 (98) = happyShift action_57
action_114 (100) = happyShift action_101
action_114 (101) = happyShift action_102
action_114 (17) = happyGoto action_77
action_114 (20) = happyGoto action_78
action_114 (21) = happyGoto action_79
action_114 (23) = happyGoto action_80
action_114 (25) = happyGoto action_81
action_114 (27) = happyGoto action_82
action_114 (30) = happyGoto action_132
action_114 (31) = happyGoto action_84
action_114 (32) = happyGoto action_85
action_114 (33) = happyGoto action_86
action_114 _ = happyFail

action_115 (51) = happyShift action_38
action_115 (52) = happyShift action_87
action_115 (55) = happyShift action_88
action_115 (61) = happyShift action_89
action_115 (63) = happyShift action_90
action_115 (71) = happyShift action_91
action_115 (72) = happyShift action_92
action_115 (73) = happyShift action_93
action_115 (74) = happyShift action_94
action_115 (75) = happyShift action_95
action_115 (76) = happyShift action_96
action_115 (77) = happyShift action_97
action_115 (81) = happyShift action_98
action_115 (83) = happyShift action_99
action_115 (85) = happyShift action_100
action_115 (89) = happyShift action_52
action_115 (93) = happyShift action_53
action_115 (94) = happyShift action_54
action_115 (95) = happyShift action_55
action_115 (97) = happyShift action_56
action_115 (98) = happyShift action_57
action_115 (100) = happyShift action_101
action_115 (101) = happyShift action_102
action_115 (17) = happyGoto action_77
action_115 (20) = happyGoto action_78
action_115 (21) = happyGoto action_79
action_115 (23) = happyGoto action_80
action_115 (25) = happyGoto action_81
action_115 (27) = happyGoto action_82
action_115 (30) = happyGoto action_131
action_115 (31) = happyGoto action_84
action_115 (32) = happyGoto action_85
action_115 (33) = happyGoto action_86
action_115 _ = happyFail

action_116 (86) = happyShift action_130
action_116 _ = happyReduce_36

action_117 (88) = happyShift action_129
action_117 _ = happyFail

action_118 (51) = happyShift action_38
action_118 (17) = happyGoto action_128
action_118 _ = happyFail

action_119 (51) = happyShift action_38
action_119 (17) = happyGoto action_116
action_119 (19) = happyGoto action_127
action_119 _ = happyFail

action_120 (88) = happyShift action_126
action_120 _ = happyFail

action_121 (51) = happyShift action_38
action_121 (52) = happyShift action_87
action_121 (55) = happyShift action_88
action_121 (61) = happyShift action_89
action_121 (63) = happyShift action_90
action_121 (71) = happyShift action_91
action_121 (72) = happyShift action_92
action_121 (73) = happyShift action_93
action_121 (74) = happyShift action_94
action_121 (75) = happyShift action_95
action_121 (76) = happyShift action_96
action_121 (77) = happyShift action_97
action_121 (81) = happyShift action_98
action_121 (83) = happyShift action_99
action_121 (85) = happyShift action_100
action_121 (89) = happyShift action_52
action_121 (93) = happyShift action_53
action_121 (94) = happyShift action_54
action_121 (95) = happyShift action_55
action_121 (97) = happyShift action_56
action_121 (98) = happyShift action_57
action_121 (100) = happyShift action_101
action_121 (101) = happyShift action_102
action_121 (17) = happyGoto action_77
action_121 (20) = happyGoto action_78
action_121 (21) = happyGoto action_79
action_121 (23) = happyGoto action_80
action_121 (25) = happyGoto action_81
action_121 (27) = happyGoto action_82
action_121 (30) = happyGoto action_125
action_121 (31) = happyGoto action_84
action_121 (32) = happyGoto action_85
action_121 (33) = happyGoto action_86
action_121 _ = happyFail

action_122 _ = happyReduce_141

action_123 (51) = happyShift action_38
action_123 (17) = happyGoto action_116
action_123 (19) = happyGoto action_124
action_123 _ = happyFail

action_124 (88) = happyShift action_226
action_124 _ = happyFail

action_125 (79) = happyShift action_225
action_125 _ = happyFail

action_126 (51) = happyShift action_38
action_126 (52) = happyShift action_87
action_126 (55) = happyShift action_88
action_126 (61) = happyShift action_89
action_126 (63) = happyShift action_90
action_126 (71) = happyShift action_91
action_126 (72) = happyShift action_92
action_126 (73) = happyShift action_93
action_126 (74) = happyShift action_94
action_126 (75) = happyShift action_95
action_126 (76) = happyShift action_96
action_126 (77) = happyShift action_97
action_126 (81) = happyShift action_98
action_126 (83) = happyShift action_99
action_126 (85) = happyShift action_100
action_126 (89) = happyShift action_52
action_126 (93) = happyShift action_53
action_126 (94) = happyShift action_54
action_126 (95) = happyShift action_55
action_126 (97) = happyShift action_56
action_126 (98) = happyShift action_57
action_126 (100) = happyShift action_101
action_126 (101) = happyShift action_102
action_126 (17) = happyGoto action_77
action_126 (20) = happyGoto action_78
action_126 (21) = happyGoto action_79
action_126 (23) = happyGoto action_80
action_126 (25) = happyGoto action_81
action_126 (27) = happyGoto action_82
action_126 (30) = happyGoto action_224
action_126 (31) = happyGoto action_84
action_126 (32) = happyGoto action_85
action_126 (33) = happyGoto action_86
action_126 _ = happyFail

action_127 (88) = happyShift action_223
action_127 _ = happyFail

action_128 (84) = happyShift action_222
action_128 _ = happyFail

action_129 (51) = happyShift action_38
action_129 (52) = happyShift action_87
action_129 (55) = happyShift action_88
action_129 (61) = happyShift action_89
action_129 (63) = happyShift action_90
action_129 (71) = happyShift action_91
action_129 (72) = happyShift action_92
action_129 (73) = happyShift action_93
action_129 (74) = happyShift action_94
action_129 (75) = happyShift action_95
action_129 (76) = happyShift action_96
action_129 (77) = happyShift action_97
action_129 (81) = happyShift action_98
action_129 (83) = happyShift action_99
action_129 (85) = happyShift action_100
action_129 (89) = happyShift action_52
action_129 (93) = happyShift action_53
action_129 (94) = happyShift action_54
action_129 (95) = happyShift action_55
action_129 (97) = happyShift action_56
action_129 (98) = happyShift action_57
action_129 (100) = happyShift action_101
action_129 (101) = happyShift action_102
action_129 (17) = happyGoto action_77
action_129 (20) = happyGoto action_78
action_129 (21) = happyGoto action_79
action_129 (23) = happyGoto action_80
action_129 (25) = happyGoto action_81
action_129 (27) = happyGoto action_82
action_129 (30) = happyGoto action_221
action_129 (31) = happyGoto action_84
action_129 (32) = happyGoto action_85
action_129 (33) = happyGoto action_86
action_129 _ = happyFail

action_130 (51) = happyShift action_38
action_130 (17) = happyGoto action_116
action_130 (19) = happyGoto action_220
action_130 _ = happyFail

action_131 (79) = happyShift action_219
action_131 _ = happyFail

action_132 (79) = happyShift action_218
action_132 _ = happyFail

action_133 (51) = happyShift action_38
action_133 (52) = happyShift action_87
action_133 (55) = happyShift action_88
action_133 (61) = happyShift action_89
action_133 (63) = happyShift action_90
action_133 (71) = happyShift action_91
action_133 (72) = happyShift action_92
action_133 (73) = happyShift action_93
action_133 (74) = happyShift action_94
action_133 (75) = happyShift action_95
action_133 (76) = happyShift action_96
action_133 (77) = happyShift action_97
action_133 (81) = happyShift action_98
action_133 (83) = happyShift action_99
action_133 (85) = happyShift action_100
action_133 (89) = happyShift action_52
action_133 (93) = happyShift action_53
action_133 (94) = happyShift action_54
action_133 (95) = happyShift action_55
action_133 (97) = happyShift action_56
action_133 (98) = happyShift action_57
action_133 (100) = happyShift action_101
action_133 (101) = happyShift action_102
action_133 (17) = happyGoto action_77
action_133 (20) = happyGoto action_78
action_133 (21) = happyGoto action_79
action_133 (23) = happyGoto action_80
action_133 (25) = happyGoto action_81
action_133 (27) = happyGoto action_82
action_133 (30) = happyGoto action_217
action_133 (31) = happyGoto action_84
action_133 (32) = happyGoto action_85
action_133 (33) = happyGoto action_86
action_133 _ = happyFail

action_134 (51) = happyShift action_38
action_134 (52) = happyShift action_87
action_134 (55) = happyShift action_88
action_134 (61) = happyShift action_89
action_134 (63) = happyShift action_90
action_134 (71) = happyShift action_91
action_134 (72) = happyShift action_92
action_134 (73) = happyShift action_93
action_134 (74) = happyShift action_94
action_134 (75) = happyShift action_95
action_134 (76) = happyShift action_96
action_134 (77) = happyShift action_97
action_134 (81) = happyShift action_98
action_134 (83) = happyShift action_99
action_134 (85) = happyShift action_100
action_134 (89) = happyShift action_52
action_134 (93) = happyShift action_53
action_134 (94) = happyShift action_54
action_134 (95) = happyShift action_55
action_134 (97) = happyShift action_56
action_134 (98) = happyShift action_57
action_134 (100) = happyShift action_101
action_134 (101) = happyShift action_102
action_134 (17) = happyGoto action_77
action_134 (20) = happyGoto action_78
action_134 (21) = happyGoto action_79
action_134 (23) = happyGoto action_80
action_134 (25) = happyGoto action_81
action_134 (27) = happyGoto action_82
action_134 (30) = happyGoto action_216
action_134 (31) = happyGoto action_84
action_134 (32) = happyGoto action_85
action_134 (33) = happyGoto action_86
action_134 _ = happyFail

action_135 _ = happyReduce_26

action_136 (51) = happyShift action_38
action_136 (17) = happyGoto action_106
action_136 (40) = happyGoto action_215
action_136 _ = happyReduce_132

action_137 _ = happyReduce_27

action_138 (92) = happyShift action_214
action_138 _ = happyReduce_112

action_139 (51) = happyShift action_38
action_139 (75) = happyShift action_211
action_139 (83) = happyShift action_212
action_139 (89) = happyShift action_213
action_139 (17) = happyGoto action_209
action_139 (43) = happyGoto action_210
action_139 _ = happyReduce_113

action_140 (51) = happyShift action_38
action_140 (52) = happyShift action_87
action_140 (55) = happyShift action_88
action_140 (61) = happyShift action_89
action_140 (63) = happyShift action_90
action_140 (71) = happyShift action_91
action_140 (72) = happyShift action_92
action_140 (73) = happyShift action_93
action_140 (74) = happyShift action_94
action_140 (75) = happyShift action_95
action_140 (76) = happyShift action_96
action_140 (77) = happyShift action_97
action_140 (81) = happyShift action_98
action_140 (83) = happyShift action_99
action_140 (85) = happyShift action_100
action_140 (89) = happyShift action_52
action_140 (93) = happyShift action_53
action_140 (94) = happyShift action_54
action_140 (95) = happyShift action_55
action_140 (97) = happyShift action_56
action_140 (98) = happyShift action_57
action_140 (100) = happyShift action_101
action_140 (101) = happyShift action_102
action_140 (17) = happyGoto action_77
action_140 (20) = happyGoto action_78
action_140 (21) = happyGoto action_79
action_140 (23) = happyGoto action_80
action_140 (25) = happyGoto action_81
action_140 (27) = happyGoto action_82
action_140 (29) = happyGoto action_208
action_140 (30) = happyGoto action_105
action_140 (31) = happyGoto action_84
action_140 (32) = happyGoto action_85
action_140 (33) = happyGoto action_86
action_140 _ = happyFail

action_141 (51) = happyShift action_38
action_141 (17) = happyGoto action_141
action_141 (18) = happyGoto action_207
action_141 _ = happyReduce_34

action_142 (90) = happyShift action_206
action_142 _ = happyFail

action_143 _ = happyReduce_44

action_144 (85) = happyShift action_204
action_144 (86) = happyShift action_205
action_144 _ = happyFail

action_145 (77) = happyShift action_202
action_145 (91) = happyShift action_203
action_145 _ = happyReduce_89

action_146 (88) = happyShift action_201
action_146 _ = happyFail

action_147 (84) = happyShift action_200
action_147 _ = happyFail

action_148 (88) = happyReduce_49
action_148 _ = happyReduce_67

action_149 (77) = happyShift action_198
action_149 (86) = happyShift action_130
action_149 (88) = happyReduce_36
action_149 (91) = happyShift action_199
action_149 _ = happyReduce_89

action_150 (88) = happyShift action_197
action_150 _ = happyFail

action_151 (82) = happyShift action_196
action_151 _ = happyFail

action_152 (88) = happyShift action_195
action_152 _ = happyFail

action_153 _ = happyReduce_93

action_154 _ = happyReduce_94

action_155 (51) = happyShift action_38
action_155 (52) = happyShift action_87
action_155 (55) = happyShift action_88
action_155 (61) = happyShift action_89
action_155 (63) = happyShift action_90
action_155 (71) = happyShift action_91
action_155 (72) = happyShift action_92
action_155 (73) = happyShift action_93
action_155 (74) = happyShift action_94
action_155 (75) = happyShift action_95
action_155 (76) = happyShift action_96
action_155 (77) = happyShift action_97
action_155 (81) = happyShift action_98
action_155 (83) = happyShift action_99
action_155 (85) = happyShift action_100
action_155 (89) = happyShift action_52
action_155 (93) = happyShift action_53
action_155 (94) = happyShift action_54
action_155 (95) = happyShift action_55
action_155 (97) = happyShift action_56
action_155 (98) = happyShift action_57
action_155 (100) = happyShift action_101
action_155 (101) = happyShift action_102
action_155 (17) = happyGoto action_77
action_155 (20) = happyGoto action_78
action_155 (21) = happyGoto action_79
action_155 (23) = happyGoto action_80
action_155 (25) = happyGoto action_81
action_155 (27) = happyGoto action_82
action_155 (28) = happyGoto action_147
action_155 (29) = happyGoto action_104
action_155 (30) = happyGoto action_105
action_155 (31) = happyGoto action_84
action_155 (32) = happyGoto action_85
action_155 (33) = happyGoto action_86
action_155 _ = happyFail

action_156 _ = happyReduce_77

action_157 _ = happyReduce_79

action_158 (88) = happyShift action_194
action_158 _ = happyFail

action_159 (83) = happyShift action_193
action_159 _ = happyFail

action_160 (92) = happyShift action_192
action_160 _ = happyFail

action_161 (51) = happyShift action_38
action_161 (17) = happyGoto action_191
action_161 _ = happyFail

action_162 (51) = happyShift action_38
action_162 (17) = happyGoto action_190
action_162 _ = happyFail

action_163 (79) = happyShift action_189
action_163 _ = happyFail

action_164 (51) = happyShift action_38
action_164 (17) = happyGoto action_141
action_164 (18) = happyGoto action_186
action_164 (34) = happyGoto action_187
action_164 (35) = happyGoto action_188
action_164 _ = happyReduce_98

action_165 (51) = happyShift action_38
action_165 (52) = happyShift action_87
action_165 (55) = happyShift action_88
action_165 (71) = happyShift action_91
action_165 (72) = happyShift action_92
action_165 (73) = happyShift action_93
action_165 (74) = happyShift action_94
action_165 (75) = happyShift action_95
action_165 (76) = happyShift action_96
action_165 (77) = happyShift action_97
action_165 (81) = happyShift action_98
action_165 (83) = happyShift action_99
action_165 (85) = happyShift action_100
action_165 (89) = happyShift action_52
action_165 (93) = happyShift action_53
action_165 (94) = happyShift action_54
action_165 (95) = happyShift action_55
action_165 (97) = happyShift action_56
action_165 (98) = happyShift action_57
action_165 (101) = happyShift action_102
action_165 (17) = happyGoto action_77
action_165 (20) = happyGoto action_78
action_165 (21) = happyGoto action_79
action_165 (23) = happyGoto action_80
action_165 (25) = happyGoto action_81
action_165 (27) = happyGoto action_174
action_165 (31) = happyGoto action_185
action_165 (32) = happyGoto action_85
action_165 (33) = happyGoto action_86
action_165 _ = happyFail

action_166 _ = happyReduce_83

action_167 _ = happyReduce_85

action_168 (51) = happyShift action_38
action_168 (17) = happyGoto action_184
action_168 _ = happyFail

action_169 (51) = happyShift action_38
action_169 (52) = happyShift action_87
action_169 (55) = happyShift action_88
action_169 (61) = happyShift action_89
action_169 (63) = happyShift action_90
action_169 (71) = happyShift action_91
action_169 (72) = happyShift action_92
action_169 (73) = happyShift action_93
action_169 (74) = happyShift action_94
action_169 (75) = happyShift action_95
action_169 (76) = happyShift action_96
action_169 (77) = happyShift action_97
action_169 (81) = happyShift action_98
action_169 (83) = happyShift action_99
action_169 (85) = happyShift action_100
action_169 (89) = happyShift action_52
action_169 (93) = happyShift action_53
action_169 (94) = happyShift action_54
action_169 (95) = happyShift action_55
action_169 (97) = happyShift action_56
action_169 (98) = happyShift action_57
action_169 (100) = happyShift action_101
action_169 (101) = happyShift action_102
action_169 (17) = happyGoto action_77
action_169 (20) = happyGoto action_78
action_169 (21) = happyGoto action_79
action_169 (23) = happyGoto action_80
action_169 (25) = happyGoto action_81
action_169 (27) = happyGoto action_82
action_169 (30) = happyGoto action_183
action_169 (31) = happyGoto action_84
action_169 (32) = happyGoto action_85
action_169 (33) = happyGoto action_86
action_169 _ = happyFail

action_170 (51) = happyShift action_38
action_170 (52) = happyShift action_87
action_170 (55) = happyShift action_88
action_170 (61) = happyShift action_89
action_170 (63) = happyShift action_90
action_170 (71) = happyShift action_91
action_170 (72) = happyShift action_92
action_170 (73) = happyShift action_93
action_170 (74) = happyShift action_94
action_170 (75) = happyShift action_95
action_170 (76) = happyShift action_96
action_170 (77) = happyShift action_97
action_170 (81) = happyShift action_98
action_170 (83) = happyShift action_99
action_170 (85) = happyShift action_100
action_170 (89) = happyShift action_52
action_170 (93) = happyShift action_53
action_170 (94) = happyShift action_54
action_170 (95) = happyShift action_55
action_170 (97) = happyShift action_56
action_170 (98) = happyShift action_57
action_170 (100) = happyShift action_101
action_170 (101) = happyShift action_102
action_170 (17) = happyGoto action_77
action_170 (20) = happyGoto action_78
action_170 (21) = happyGoto action_79
action_170 (23) = happyGoto action_80
action_170 (25) = happyGoto action_81
action_170 (27) = happyGoto action_82
action_170 (30) = happyGoto action_182
action_170 (31) = happyGoto action_84
action_170 (32) = happyGoto action_85
action_170 (33) = happyGoto action_86
action_170 _ = happyFail

action_171 (51) = happyShift action_38
action_171 (52) = happyShift action_87
action_171 (55) = happyShift action_88
action_171 (71) = happyShift action_91
action_171 (72) = happyShift action_92
action_171 (73) = happyShift action_93
action_171 (74) = happyShift action_94
action_171 (75) = happyShift action_95
action_171 (76) = happyShift action_96
action_171 (77) = happyShift action_97
action_171 (81) = happyShift action_98
action_171 (83) = happyShift action_99
action_171 (85) = happyShift action_100
action_171 (89) = happyShift action_52
action_171 (93) = happyShift action_53
action_171 (94) = happyShift action_54
action_171 (95) = happyShift action_55
action_171 (97) = happyShift action_56
action_171 (98) = happyShift action_57
action_171 (101) = happyShift action_102
action_171 (17) = happyGoto action_77
action_171 (20) = happyGoto action_78
action_171 (21) = happyGoto action_79
action_171 (23) = happyGoto action_80
action_171 (25) = happyGoto action_81
action_171 (27) = happyGoto action_174
action_171 (31) = happyGoto action_181
action_171 (32) = happyGoto action_85
action_171 (33) = happyGoto action_86
action_171 _ = happyFail

action_172 (85) = happyShift action_100
action_172 (21) = happyGoto action_180
action_172 _ = happyFail

action_173 (85) = happyShift action_100
action_173 (21) = happyGoto action_179
action_173 _ = happyFail

action_174 (99) = happyShift action_171
action_174 _ = happyFail

action_175 (99) = happyReduce_63
action_175 _ = happyReduce_63

action_176 (51) = happyShift action_38
action_176 (52) = happyShift action_87
action_176 (55) = happyShift action_88
action_176 (61) = happyShift action_89
action_176 (63) = happyShift action_90
action_176 (71) = happyShift action_91
action_176 (72) = happyShift action_92
action_176 (73) = happyShift action_93
action_176 (74) = happyShift action_94
action_176 (75) = happyShift action_95
action_176 (76) = happyShift action_96
action_176 (77) = happyShift action_97
action_176 (81) = happyShift action_98
action_176 (83) = happyShift action_99
action_176 (85) = happyShift action_100
action_176 (89) = happyShift action_52
action_176 (93) = happyShift action_53
action_176 (94) = happyShift action_54
action_176 (95) = happyShift action_55
action_176 (97) = happyShift action_56
action_176 (98) = happyShift action_57
action_176 (100) = happyShift action_101
action_176 (101) = happyShift action_102
action_176 (17) = happyGoto action_145
action_176 (20) = happyGoto action_78
action_176 (21) = happyGoto action_79
action_176 (23) = happyGoto action_80
action_176 (24) = happyGoto action_178
action_176 (25) = happyGoto action_81
action_176 (27) = happyGoto action_82
action_176 (28) = happyGoto action_147
action_176 (29) = happyGoto action_148
action_176 (30) = happyGoto action_105
action_176 (31) = happyGoto action_84
action_176 (32) = happyGoto action_85
action_176 (33) = happyGoto action_86
action_176 _ = happyFail

action_177 _ = happyReduce_30

action_178 (88) = happyShift action_267
action_178 _ = happyFail

action_179 _ = happyReduce_48

action_180 _ = happyReduce_47

action_181 (99) = happyReduce_81
action_181 _ = happyReduce_81

action_182 _ = happyReduce_70

action_183 _ = happyReduce_75

action_184 _ = happyReduce_84

action_185 (99) = happyReduce_80
action_185 _ = happyReduce_80

action_186 (92) = happyShift action_266
action_186 _ = happyFail

action_187 (80) = happyShift action_265
action_187 _ = happyFail

action_188 (87) = happyShift action_264
action_188 _ = happyReduce_97

action_189 (51) = happyShift action_38
action_189 (75) = happyShift action_211
action_189 (83) = happyShift action_212
action_189 (89) = happyShift action_213
action_189 (17) = happyGoto action_209
action_189 (39) = happyGoto action_262
action_189 (43) = happyGoto action_263
action_189 _ = happyReduce_110

action_190 (88) = happyShift action_261
action_190 _ = happyFail

action_191 (88) = happyShift action_260
action_191 _ = happyFail

action_192 (51) = happyShift action_38
action_192 (52) = happyShift action_87
action_192 (55) = happyShift action_88
action_192 (61) = happyShift action_89
action_192 (63) = happyShift action_90
action_192 (71) = happyShift action_91
action_192 (72) = happyShift action_92
action_192 (73) = happyShift action_93
action_192 (74) = happyShift action_94
action_192 (75) = happyShift action_95
action_192 (76) = happyShift action_96
action_192 (77) = happyShift action_97
action_192 (81) = happyShift action_98
action_192 (83) = happyShift action_99
action_192 (85) = happyShift action_100
action_192 (89) = happyShift action_52
action_192 (93) = happyShift action_53
action_192 (94) = happyShift action_54
action_192 (95) = happyShift action_55
action_192 (97) = happyShift action_56
action_192 (98) = happyShift action_57
action_192 (100) = happyShift action_101
action_192 (101) = happyShift action_102
action_192 (17) = happyGoto action_77
action_192 (20) = happyGoto action_78
action_192 (21) = happyGoto action_79
action_192 (23) = happyGoto action_80
action_192 (25) = happyGoto action_81
action_192 (27) = happyGoto action_82
action_192 (28) = happyGoto action_259
action_192 (29) = happyGoto action_104
action_192 (30) = happyGoto action_105
action_192 (31) = happyGoto action_84
action_192 (32) = happyGoto action_85
action_192 (33) = happyGoto action_86
action_192 _ = happyFail

action_193 (51) = happyShift action_38
action_193 (17) = happyGoto action_258
action_193 _ = happyFail

action_194 (51) = happyShift action_38
action_194 (52) = happyShift action_87
action_194 (55) = happyShift action_88
action_194 (61) = happyShift action_89
action_194 (63) = happyShift action_90
action_194 (71) = happyShift action_91
action_194 (72) = happyShift action_92
action_194 (73) = happyShift action_93
action_194 (74) = happyShift action_94
action_194 (75) = happyShift action_95
action_194 (76) = happyShift action_96
action_194 (77) = happyShift action_97
action_194 (81) = happyShift action_98
action_194 (83) = happyShift action_99
action_194 (85) = happyShift action_100
action_194 (89) = happyShift action_52
action_194 (93) = happyShift action_53
action_194 (94) = happyShift action_54
action_194 (95) = happyShift action_55
action_194 (97) = happyShift action_56
action_194 (98) = happyShift action_57
action_194 (100) = happyShift action_101
action_194 (101) = happyShift action_102
action_194 (17) = happyGoto action_77
action_194 (20) = happyGoto action_78
action_194 (21) = happyGoto action_79
action_194 (23) = happyGoto action_80
action_194 (25) = happyGoto action_81
action_194 (27) = happyGoto action_82
action_194 (30) = happyGoto action_257
action_194 (31) = happyGoto action_84
action_194 (32) = happyGoto action_85
action_194 (33) = happyGoto action_86
action_194 _ = happyFail

action_195 (51) = happyShift action_38
action_195 (52) = happyShift action_87
action_195 (55) = happyShift action_88
action_195 (61) = happyShift action_89
action_195 (63) = happyShift action_90
action_195 (71) = happyShift action_91
action_195 (72) = happyShift action_92
action_195 (73) = happyShift action_93
action_195 (74) = happyShift action_94
action_195 (75) = happyShift action_95
action_195 (76) = happyShift action_96
action_195 (77) = happyShift action_97
action_195 (81) = happyShift action_98
action_195 (83) = happyShift action_99
action_195 (85) = happyShift action_100
action_195 (89) = happyShift action_52
action_195 (93) = happyShift action_53
action_195 (94) = happyShift action_54
action_195 (95) = happyShift action_55
action_195 (97) = happyShift action_56
action_195 (98) = happyShift action_57
action_195 (100) = happyShift action_101
action_195 (101) = happyShift action_102
action_195 (17) = happyGoto action_77
action_195 (20) = happyGoto action_78
action_195 (21) = happyGoto action_79
action_195 (23) = happyGoto action_80
action_195 (25) = happyGoto action_81
action_195 (27) = happyGoto action_82
action_195 (30) = happyGoto action_256
action_195 (31) = happyGoto action_84
action_195 (32) = happyGoto action_85
action_195 (33) = happyGoto action_86
action_195 _ = happyFail

action_196 _ = happyReduce_62

action_197 (51) = happyShift action_38
action_197 (52) = happyShift action_87
action_197 (55) = happyShift action_88
action_197 (61) = happyShift action_89
action_197 (63) = happyShift action_90
action_197 (71) = happyShift action_91
action_197 (72) = happyShift action_92
action_197 (73) = happyShift action_93
action_197 (74) = happyShift action_94
action_197 (75) = happyShift action_95
action_197 (76) = happyShift action_96
action_197 (77) = happyShift action_97
action_197 (81) = happyShift action_98
action_197 (83) = happyShift action_99
action_197 (85) = happyShift action_100
action_197 (89) = happyShift action_52
action_197 (93) = happyShift action_53
action_197 (94) = happyShift action_54
action_197 (95) = happyShift action_55
action_197 (97) = happyShift action_56
action_197 (98) = happyShift action_57
action_197 (100) = happyShift action_101
action_197 (101) = happyShift action_102
action_197 (17) = happyGoto action_77
action_197 (20) = happyGoto action_78
action_197 (21) = happyGoto action_79
action_197 (23) = happyGoto action_80
action_197 (25) = happyGoto action_81
action_197 (27) = happyGoto action_82
action_197 (30) = happyGoto action_255
action_197 (31) = happyGoto action_84
action_197 (32) = happyGoto action_85
action_197 (33) = happyGoto action_86
action_197 _ = happyFail

action_198 (51) = happyShift action_38
action_198 (52) = happyShift action_87
action_198 (55) = happyShift action_88
action_198 (61) = happyShift action_89
action_198 (63) = happyShift action_90
action_198 (71) = happyShift action_91
action_198 (72) = happyShift action_92
action_198 (73) = happyShift action_93
action_198 (74) = happyShift action_94
action_198 (75) = happyShift action_95
action_198 (76) = happyShift action_96
action_198 (77) = happyShift action_97
action_198 (81) = happyShift action_98
action_198 (83) = happyShift action_99
action_198 (85) = happyShift action_100
action_198 (89) = happyShift action_52
action_198 (93) = happyShift action_53
action_198 (94) = happyShift action_54
action_198 (95) = happyShift action_55
action_198 (97) = happyShift action_56
action_198 (98) = happyShift action_57
action_198 (100) = happyShift action_101
action_198 (101) = happyShift action_102
action_198 (17) = happyGoto action_77
action_198 (20) = happyGoto action_78
action_198 (21) = happyGoto action_79
action_198 (23) = happyGoto action_80
action_198 (25) = happyGoto action_81
action_198 (27) = happyGoto action_82
action_198 (30) = happyGoto action_254
action_198 (31) = happyGoto action_84
action_198 (32) = happyGoto action_85
action_198 (33) = happyGoto action_86
action_198 _ = happyFail

action_199 (51) = happyShift action_38
action_199 (52) = happyShift action_87
action_199 (55) = happyShift action_88
action_199 (61) = happyShift action_89
action_199 (63) = happyShift action_90
action_199 (71) = happyShift action_91
action_199 (72) = happyShift action_92
action_199 (73) = happyShift action_93
action_199 (74) = happyShift action_94
action_199 (75) = happyShift action_95
action_199 (76) = happyShift action_96
action_199 (77) = happyShift action_97
action_199 (81) = happyShift action_98
action_199 (83) = happyShift action_99
action_199 (85) = happyShift action_100
action_199 (89) = happyShift action_52
action_199 (93) = happyShift action_53
action_199 (94) = happyShift action_54
action_199 (95) = happyShift action_55
action_199 (97) = happyShift action_56
action_199 (98) = happyShift action_57
action_199 (100) = happyShift action_101
action_199 (101) = happyShift action_102
action_199 (17) = happyGoto action_77
action_199 (20) = happyGoto action_78
action_199 (21) = happyGoto action_79
action_199 (23) = happyGoto action_80
action_199 (25) = happyGoto action_81
action_199 (27) = happyGoto action_82
action_199 (30) = happyGoto action_253
action_199 (31) = happyGoto action_84
action_199 (32) = happyGoto action_85
action_199 (33) = happyGoto action_86
action_199 _ = happyFail

action_200 _ = happyReduce_91

action_201 (51) = happyShift action_38
action_201 (52) = happyShift action_87
action_201 (55) = happyShift action_88
action_201 (61) = happyShift action_89
action_201 (63) = happyShift action_90
action_201 (71) = happyShift action_91
action_201 (72) = happyShift action_92
action_201 (73) = happyShift action_93
action_201 (74) = happyShift action_94
action_201 (75) = happyShift action_95
action_201 (76) = happyShift action_96
action_201 (77) = happyShift action_97
action_201 (81) = happyShift action_98
action_201 (83) = happyShift action_99
action_201 (85) = happyShift action_100
action_201 (89) = happyShift action_52
action_201 (93) = happyShift action_53
action_201 (94) = happyShift action_54
action_201 (95) = happyShift action_55
action_201 (97) = happyShift action_56
action_201 (98) = happyShift action_57
action_201 (100) = happyShift action_101
action_201 (101) = happyShift action_102
action_201 (17) = happyGoto action_77
action_201 (20) = happyGoto action_78
action_201 (21) = happyGoto action_79
action_201 (23) = happyGoto action_80
action_201 (25) = happyGoto action_81
action_201 (27) = happyGoto action_82
action_201 (30) = happyGoto action_252
action_201 (31) = happyGoto action_84
action_201 (32) = happyGoto action_85
action_201 (33) = happyGoto action_86
action_201 _ = happyFail

action_202 (51) = happyShift action_38
action_202 (52) = happyShift action_87
action_202 (55) = happyShift action_88
action_202 (61) = happyShift action_89
action_202 (63) = happyShift action_90
action_202 (71) = happyShift action_91
action_202 (72) = happyShift action_92
action_202 (73) = happyShift action_93
action_202 (74) = happyShift action_94
action_202 (75) = happyShift action_95
action_202 (76) = happyShift action_96
action_202 (77) = happyShift action_97
action_202 (81) = happyShift action_98
action_202 (83) = happyShift action_99
action_202 (85) = happyShift action_100
action_202 (89) = happyShift action_52
action_202 (93) = happyShift action_53
action_202 (94) = happyShift action_54
action_202 (95) = happyShift action_55
action_202 (97) = happyShift action_56
action_202 (98) = happyShift action_57
action_202 (100) = happyShift action_101
action_202 (101) = happyShift action_102
action_202 (17) = happyGoto action_77
action_202 (20) = happyGoto action_78
action_202 (21) = happyGoto action_79
action_202 (23) = happyGoto action_80
action_202 (25) = happyGoto action_81
action_202 (27) = happyGoto action_82
action_202 (30) = happyGoto action_251
action_202 (31) = happyGoto action_84
action_202 (32) = happyGoto action_85
action_202 (33) = happyGoto action_86
action_202 _ = happyFail

action_203 (51) = happyShift action_38
action_203 (52) = happyShift action_87
action_203 (55) = happyShift action_88
action_203 (61) = happyShift action_89
action_203 (63) = happyShift action_90
action_203 (71) = happyShift action_91
action_203 (72) = happyShift action_92
action_203 (73) = happyShift action_93
action_203 (74) = happyShift action_94
action_203 (75) = happyShift action_95
action_203 (76) = happyShift action_96
action_203 (77) = happyShift action_97
action_203 (81) = happyShift action_98
action_203 (83) = happyShift action_99
action_203 (85) = happyShift action_100
action_203 (89) = happyShift action_52
action_203 (93) = happyShift action_53
action_203 (94) = happyShift action_54
action_203 (95) = happyShift action_55
action_203 (97) = happyShift action_56
action_203 (98) = happyShift action_57
action_203 (100) = happyShift action_101
action_203 (101) = happyShift action_102
action_203 (17) = happyGoto action_77
action_203 (20) = happyGoto action_78
action_203 (21) = happyGoto action_79
action_203 (23) = happyGoto action_80
action_203 (25) = happyGoto action_81
action_203 (27) = happyGoto action_82
action_203 (30) = happyGoto action_250
action_203 (31) = happyGoto action_84
action_203 (32) = happyGoto action_85
action_203 (33) = happyGoto action_86
action_203 _ = happyFail

action_204 _ = happyReduce_45

action_205 (51) = happyShift action_38
action_205 (52) = happyShift action_87
action_205 (55) = happyShift action_88
action_205 (61) = happyShift action_89
action_205 (63) = happyShift action_90
action_205 (71) = happyShift action_91
action_205 (72) = happyShift action_92
action_205 (73) = happyShift action_93
action_205 (74) = happyShift action_94
action_205 (75) = happyShift action_95
action_205 (76) = happyShift action_96
action_205 (77) = happyShift action_97
action_205 (81) = happyShift action_98
action_205 (83) = happyShift action_99
action_205 (85) = happyShift action_100
action_205 (89) = happyShift action_52
action_205 (93) = happyShift action_53
action_205 (94) = happyShift action_54
action_205 (95) = happyShift action_55
action_205 (97) = happyShift action_56
action_205 (98) = happyShift action_57
action_205 (100) = happyShift action_101
action_205 (101) = happyShift action_102
action_205 (17) = happyGoto action_77
action_205 (20) = happyGoto action_78
action_205 (21) = happyGoto action_79
action_205 (22) = happyGoto action_249
action_205 (23) = happyGoto action_80
action_205 (25) = happyGoto action_81
action_205 (27) = happyGoto action_82
action_205 (30) = happyGoto action_144
action_205 (31) = happyGoto action_84
action_205 (32) = happyGoto action_85
action_205 (33) = happyGoto action_86
action_205 _ = happyFail

action_206 (51) = happyShift action_38
action_206 (52) = happyShift action_87
action_206 (55) = happyShift action_88
action_206 (61) = happyShift action_89
action_206 (63) = happyShift action_90
action_206 (71) = happyShift action_91
action_206 (72) = happyShift action_92
action_206 (73) = happyShift action_93
action_206 (74) = happyShift action_94
action_206 (75) = happyShift action_95
action_206 (76) = happyShift action_96
action_206 (77) = happyShift action_97
action_206 (81) = happyShift action_98
action_206 (83) = happyShift action_99
action_206 (85) = happyShift action_100
action_206 (89) = happyShift action_52
action_206 (93) = happyShift action_53
action_206 (94) = happyShift action_54
action_206 (95) = happyShift action_55
action_206 (97) = happyShift action_56
action_206 (98) = happyShift action_57
action_206 (100) = happyShift action_101
action_206 (101) = happyShift action_102
action_206 (17) = happyGoto action_77
action_206 (20) = happyGoto action_78
action_206 (21) = happyGoto action_79
action_206 (23) = happyGoto action_80
action_206 (25) = happyGoto action_81
action_206 (27) = happyGoto action_82
action_206 (28) = happyGoto action_248
action_206 (29) = happyGoto action_104
action_206 (30) = happyGoto action_105
action_206 (31) = happyGoto action_84
action_206 (32) = happyGoto action_85
action_206 (33) = happyGoto action_86
action_206 _ = happyFail

action_207 _ = happyReduce_35

action_208 _ = happyReduce_69

action_209 _ = happyReduce_118

action_210 _ = happyReduce_115

action_211 (51) = happyShift action_38
action_211 (75) = happyShift action_211
action_211 (83) = happyShift action_212
action_211 (89) = happyShift action_213
action_211 (17) = happyGoto action_209
action_211 (43) = happyGoto action_247
action_211 _ = happyFail

action_212 (51) = happyShift action_38
action_212 (75) = happyShift action_211
action_212 (83) = happyShift action_212
action_212 (84) = happyShift action_246
action_212 (89) = happyShift action_213
action_212 (17) = happyGoto action_241
action_212 (43) = happyGoto action_242
action_212 (44) = happyGoto action_243
action_212 (45) = happyGoto action_244
action_212 (46) = happyGoto action_245
action_212 _ = happyFail

action_213 (51) = happyShift action_38
action_213 (52) = happyShift action_154
action_213 (55) = happyShift action_88
action_213 (71) = happyShift action_240
action_213 (73) = happyShift action_93
action_213 (74) = happyShift action_94
action_213 (75) = happyShift action_95
action_213 (76) = happyShift action_96
action_213 (77) = happyShift action_97
action_213 (83) = happyShift action_155
action_213 (101) = happyShift action_102
action_213 (17) = happyGoto action_77
action_213 (33) = happyGoto action_239
action_213 _ = happyFail

action_214 (51) = happyShift action_38
action_214 (52) = happyShift action_87
action_214 (55) = happyShift action_88
action_214 (61) = happyShift action_89
action_214 (63) = happyShift action_90
action_214 (71) = happyShift action_91
action_214 (72) = happyShift action_92
action_214 (73) = happyShift action_93
action_214 (74) = happyShift action_94
action_214 (75) = happyShift action_95
action_214 (76) = happyShift action_96
action_214 (77) = happyShift action_97
action_214 (81) = happyShift action_98
action_214 (83) = happyShift action_99
action_214 (85) = happyShift action_100
action_214 (89) = happyShift action_52
action_214 (93) = happyShift action_53
action_214 (94) = happyShift action_54
action_214 (95) = happyShift action_55
action_214 (97) = happyShift action_56
action_214 (98) = happyShift action_57
action_214 (100) = happyShift action_101
action_214 (101) = happyShift action_102
action_214 (17) = happyGoto action_77
action_214 (20) = happyGoto action_78
action_214 (21) = happyGoto action_79
action_214 (23) = happyGoto action_80
action_214 (25) = happyGoto action_81
action_214 (27) = happyGoto action_82
action_214 (28) = happyGoto action_238
action_214 (29) = happyGoto action_104
action_214 (30) = happyGoto action_105
action_214 (31) = happyGoto action_84
action_214 (32) = happyGoto action_85
action_214 (33) = happyGoto action_86
action_214 _ = happyFail

action_215 _ = happyReduce_131

action_216 (79) = happyShift action_237
action_216 _ = happyFail

action_217 (79) = happyShift action_236
action_217 _ = happyFail

action_218 (51) = happyShift action_38
action_218 (17) = happyGoto action_36
action_218 (36) = happyGoto action_228
action_218 (37) = happyGoto action_235
action_218 _ = happyFail

action_219 (51) = happyShift action_38
action_219 (17) = happyGoto action_36
action_219 (36) = happyGoto action_228
action_219 (37) = happyGoto action_229
action_219 (38) = happyGoto action_234
action_219 _ = happyReduce_105

action_220 _ = happyReduce_37

action_221 (84) = happyShift action_233
action_221 _ = happyFail

action_222 _ = happyReduce_139

action_223 (51) = happyShift action_38
action_223 (52) = happyShift action_87
action_223 (55) = happyShift action_88
action_223 (61) = happyShift action_89
action_223 (63) = happyShift action_90
action_223 (71) = happyShift action_91
action_223 (72) = happyShift action_92
action_223 (73) = happyShift action_93
action_223 (74) = happyShift action_94
action_223 (75) = happyShift action_95
action_223 (76) = happyShift action_96
action_223 (77) = happyShift action_97
action_223 (81) = happyShift action_98
action_223 (83) = happyShift action_99
action_223 (85) = happyShift action_100
action_223 (89) = happyShift action_52
action_223 (93) = happyShift action_53
action_223 (94) = happyShift action_54
action_223 (95) = happyShift action_55
action_223 (97) = happyShift action_56
action_223 (98) = happyShift action_57
action_223 (100) = happyShift action_101
action_223 (101) = happyShift action_102
action_223 (17) = happyGoto action_77
action_223 (20) = happyGoto action_78
action_223 (21) = happyGoto action_79
action_223 (23) = happyGoto action_80
action_223 (25) = happyGoto action_81
action_223 (27) = happyGoto action_82
action_223 (30) = happyGoto action_232
action_223 (31) = happyGoto action_84
action_223 (32) = happyGoto action_85
action_223 (33) = happyGoto action_86
action_223 _ = happyFail

action_224 (82) = happyShift action_231
action_224 _ = happyFail

action_225 (51) = happyShift action_38
action_225 (17) = happyGoto action_36
action_225 (36) = happyGoto action_228
action_225 (37) = happyGoto action_229
action_225 (38) = happyGoto action_230
action_225 _ = happyReduce_105

action_226 (51) = happyShift action_38
action_226 (52) = happyShift action_87
action_226 (55) = happyShift action_88
action_226 (61) = happyShift action_89
action_226 (63) = happyShift action_90
action_226 (71) = happyShift action_91
action_226 (72) = happyShift action_92
action_226 (73) = happyShift action_93
action_226 (74) = happyShift action_94
action_226 (75) = happyShift action_95
action_226 (76) = happyShift action_96
action_226 (77) = happyShift action_97
action_226 (81) = happyShift action_98
action_226 (83) = happyShift action_99
action_226 (85) = happyShift action_100
action_226 (89) = happyShift action_52
action_226 (93) = happyShift action_53
action_226 (94) = happyShift action_54
action_226 (95) = happyShift action_55
action_226 (97) = happyShift action_56
action_226 (98) = happyShift action_57
action_226 (100) = happyShift action_101
action_226 (101) = happyShift action_102
action_226 (17) = happyGoto action_77
action_226 (20) = happyGoto action_78
action_226 (21) = happyGoto action_79
action_226 (23) = happyGoto action_80
action_226 (25) = happyGoto action_81
action_226 (27) = happyGoto action_82
action_226 (30) = happyGoto action_227
action_226 (31) = happyGoto action_84
action_226 (32) = happyGoto action_85
action_226 (33) = happyGoto action_86
action_226 _ = happyFail

action_227 (84) = happyShift action_298
action_227 _ = happyFail

action_228 _ = happyReduce_101

action_229 _ = happyReduce_104

action_230 (80) = happyShift action_297
action_230 (87) = happyShift action_295
action_230 _ = happyFail

action_231 _ = happyReduce_136

action_232 (84) = happyShift action_296
action_232 _ = happyFail

action_233 _ = happyReduce_135

action_234 (80) = happyShift action_294
action_234 (87) = happyShift action_295
action_234 _ = happyFail

action_235 (80) = happyShift action_293
action_235 _ = happyFail

action_236 (51) = happyShift action_38
action_236 (17) = happyGoto action_36
action_236 (36) = happyGoto action_228
action_236 (37) = happyGoto action_229
action_236 (38) = happyGoto action_292
action_236 _ = happyReduce_105

action_237 (51) = happyShift action_38
action_237 (17) = happyGoto action_36
action_237 (36) = happyGoto action_228
action_237 (37) = happyGoto action_229
action_237 (38) = happyGoto action_291
action_237 _ = happyReduce_105

action_238 _ = happyReduce_111

action_239 _ = happyReduce_121

action_240 _ = happyReduce_120

action_241 (51) = happyShift action_38
action_241 (75) = happyShift action_211
action_241 (77) = happyShift action_289
action_241 (78) = happyShift action_290
action_241 (83) = happyShift action_212
action_241 (89) = happyShift action_213
action_241 (17) = happyGoto action_209
action_241 (43) = happyGoto action_288
action_241 _ = happyReduce_118

action_242 _ = happyReduce_127

action_243 (84) = happyShift action_287
action_243 _ = happyFail

action_244 (86) = happyShift action_286
action_244 _ = happyReduce_123

action_245 (51) = happyShift action_38
action_245 (75) = happyShift action_211
action_245 (83) = happyShift action_212
action_245 (89) = happyShift action_213
action_245 (17) = happyGoto action_209
action_245 (43) = happyGoto action_285
action_245 _ = happyReduce_124

action_246 _ = happyReduce_116

action_247 _ = happyReduce_119

action_248 _ = happyReduce_71

action_249 _ = happyReduce_46

action_250 (84) = happyShift action_284
action_250 _ = happyFail

action_251 (84) = happyShift action_283
action_251 _ = happyFail

action_252 (84) = happyShift action_282
action_252 _ = happyFail

action_253 (82) = happyShift action_281
action_253 _ = happyFail

action_254 (82) = happyShift action_280
action_254 _ = happyFail

action_255 (82) = happyShift action_279
action_255 _ = happyFail

action_256 (78) = happyShift action_278
action_256 _ = happyFail

action_257 _ = happyReduce_57

action_258 (88) = happyShift action_277
action_258 _ = happyFail

action_259 (64) = happyShift action_276
action_259 _ = happyFail

action_260 (51) = happyShift action_38
action_260 (52) = happyShift action_87
action_260 (55) = happyShift action_88
action_260 (61) = happyShift action_89
action_260 (63) = happyShift action_90
action_260 (71) = happyShift action_91
action_260 (72) = happyShift action_92
action_260 (73) = happyShift action_93
action_260 (74) = happyShift action_94
action_260 (75) = happyShift action_95
action_260 (76) = happyShift action_96
action_260 (77) = happyShift action_97
action_260 (81) = happyShift action_98
action_260 (83) = happyShift action_99
action_260 (85) = happyShift action_100
action_260 (89) = happyShift action_52
action_260 (93) = happyShift action_53
action_260 (94) = happyShift action_54
action_260 (95) = happyShift action_55
action_260 (97) = happyShift action_56
action_260 (98) = happyShift action_57
action_260 (100) = happyShift action_101
action_260 (101) = happyShift action_102
action_260 (17) = happyGoto action_77
action_260 (20) = happyGoto action_78
action_260 (21) = happyGoto action_79
action_260 (23) = happyGoto action_80
action_260 (25) = happyGoto action_81
action_260 (27) = happyGoto action_82
action_260 (30) = happyGoto action_275
action_260 (31) = happyGoto action_84
action_260 (32) = happyGoto action_85
action_260 (33) = happyGoto action_86
action_260 _ = happyFail

action_261 (51) = happyShift action_38
action_261 (52) = happyShift action_87
action_261 (55) = happyShift action_88
action_261 (61) = happyShift action_89
action_261 (63) = happyShift action_90
action_261 (71) = happyShift action_91
action_261 (72) = happyShift action_92
action_261 (73) = happyShift action_93
action_261 (74) = happyShift action_94
action_261 (75) = happyShift action_95
action_261 (76) = happyShift action_96
action_261 (77) = happyShift action_97
action_261 (81) = happyShift action_98
action_261 (83) = happyShift action_99
action_261 (85) = happyShift action_100
action_261 (89) = happyShift action_52
action_261 (93) = happyShift action_53
action_261 (94) = happyShift action_54
action_261 (95) = happyShift action_55
action_261 (97) = happyShift action_56
action_261 (98) = happyShift action_57
action_261 (100) = happyShift action_101
action_261 (101) = happyShift action_102
action_261 (17) = happyGoto action_77
action_261 (20) = happyGoto action_78
action_261 (21) = happyGoto action_79
action_261 (23) = happyGoto action_80
action_261 (25) = happyGoto action_81
action_261 (27) = happyGoto action_82
action_261 (30) = happyGoto action_274
action_261 (31) = happyGoto action_84
action_261 (32) = happyGoto action_85
action_261 (33) = happyGoto action_86
action_261 _ = happyFail

action_262 (80) = happyShift action_273
action_262 _ = happyFail

action_263 (87) = happyShift action_271
action_263 (90) = happyShift action_272
action_263 _ = happyReduce_109

action_264 (51) = happyShift action_38
action_264 (17) = happyGoto action_141
action_264 (18) = happyGoto action_186
action_264 (34) = happyGoto action_270
action_264 (35) = happyGoto action_188
action_264 _ = happyReduce_98

action_265 _ = happyReduce_95

action_266 (51) = happyShift action_38
action_266 (52) = happyShift action_87
action_266 (55) = happyShift action_88
action_266 (61) = happyShift action_89
action_266 (63) = happyShift action_90
action_266 (71) = happyShift action_91
action_266 (72) = happyShift action_92
action_266 (73) = happyShift action_93
action_266 (74) = happyShift action_94
action_266 (75) = happyShift action_95
action_266 (76) = happyShift action_96
action_266 (77) = happyShift action_97
action_266 (81) = happyShift action_98
action_266 (83) = happyShift action_99
action_266 (85) = happyShift action_100
action_266 (89) = happyShift action_52
action_266 (93) = happyShift action_53
action_266 (94) = happyShift action_54
action_266 (95) = happyShift action_55
action_266 (97) = happyShift action_56
action_266 (98) = happyShift action_57
action_266 (100) = happyShift action_101
action_266 (101) = happyShift action_102
action_266 (17) = happyGoto action_77
action_266 (20) = happyGoto action_78
action_266 (21) = happyGoto action_79
action_266 (23) = happyGoto action_80
action_266 (25) = happyGoto action_81
action_266 (27) = happyGoto action_82
action_266 (28) = happyGoto action_269
action_266 (29) = happyGoto action_104
action_266 (30) = happyGoto action_105
action_266 (31) = happyGoto action_84
action_266 (32) = happyGoto action_85
action_266 (33) = happyGoto action_86
action_266 _ = happyFail

action_267 (51) = happyShift action_38
action_267 (52) = happyShift action_87
action_267 (55) = happyShift action_88
action_267 (61) = happyShift action_89
action_267 (63) = happyShift action_90
action_267 (71) = happyShift action_91
action_267 (72) = happyShift action_92
action_267 (73) = happyShift action_93
action_267 (74) = happyShift action_94
action_267 (75) = happyShift action_95
action_267 (76) = happyShift action_96
action_267 (77) = happyShift action_97
action_267 (81) = happyShift action_98
action_267 (83) = happyShift action_99
action_267 (85) = happyShift action_100
action_267 (89) = happyShift action_52
action_267 (93) = happyShift action_53
action_267 (94) = happyShift action_54
action_267 (95) = happyShift action_55
action_267 (97) = happyShift action_56
action_267 (98) = happyShift action_57
action_267 (100) = happyShift action_101
action_267 (101) = happyShift action_102
action_267 (17) = happyGoto action_77
action_267 (20) = happyGoto action_78
action_267 (21) = happyGoto action_79
action_267 (23) = happyGoto action_80
action_267 (25) = happyGoto action_81
action_267 (27) = happyGoto action_82
action_267 (30) = happyGoto action_268
action_267 (31) = happyGoto action_84
action_267 (32) = happyGoto action_85
action_267 (33) = happyGoto action_86
action_267 _ = happyFail

action_268 (84) = happyShift action_315
action_268 _ = happyFail

action_269 _ = happyReduce_99

action_270 _ = happyReduce_96

action_271 (51) = happyShift action_38
action_271 (75) = happyShift action_211
action_271 (83) = happyShift action_212
action_271 (89) = happyShift action_213
action_271 (17) = happyGoto action_209
action_271 (39) = happyGoto action_314
action_271 (43) = happyGoto action_263
action_271 _ = happyReduce_110

action_272 (51) = happyShift action_38
action_272 (52) = happyShift action_87
action_272 (55) = happyShift action_88
action_272 (61) = happyShift action_89
action_272 (63) = happyShift action_90
action_272 (71) = happyShift action_91
action_272 (72) = happyShift action_92
action_272 (73) = happyShift action_93
action_272 (74) = happyShift action_94
action_272 (75) = happyShift action_95
action_272 (76) = happyShift action_96
action_272 (77) = happyShift action_97
action_272 (81) = happyShift action_98
action_272 (83) = happyShift action_99
action_272 (85) = happyShift action_100
action_272 (89) = happyShift action_52
action_272 (93) = happyShift action_53
action_272 (94) = happyShift action_54
action_272 (95) = happyShift action_55
action_272 (97) = happyShift action_56
action_272 (98) = happyShift action_57
action_272 (100) = happyShift action_101
action_272 (101) = happyShift action_102
action_272 (17) = happyGoto action_77
action_272 (20) = happyGoto action_78
action_272 (21) = happyGoto action_79
action_272 (23) = happyGoto action_80
action_272 (25) = happyGoto action_81
action_272 (27) = happyGoto action_82
action_272 (28) = happyGoto action_313
action_272 (29) = happyGoto action_104
action_272 (30) = happyGoto action_105
action_272 (31) = happyGoto action_84
action_272 (32) = happyGoto action_85
action_272 (33) = happyGoto action_86
action_272 _ = happyFail

action_273 _ = happyReduce_73

action_274 (84) = happyShift action_312
action_274 _ = happyFail

action_275 (82) = happyShift action_311
action_275 _ = happyFail

action_276 (51) = happyShift action_38
action_276 (52) = happyShift action_87
action_276 (55) = happyShift action_88
action_276 (61) = happyShift action_89
action_276 (63) = happyShift action_90
action_276 (71) = happyShift action_91
action_276 (72) = happyShift action_92
action_276 (73) = happyShift action_93
action_276 (74) = happyShift action_94
action_276 (75) = happyShift action_95
action_276 (76) = happyShift action_96
action_276 (77) = happyShift action_97
action_276 (81) = happyShift action_98
action_276 (83) = happyShift action_99
action_276 (85) = happyShift action_100
action_276 (89) = happyShift action_52
action_276 (93) = happyShift action_53
action_276 (94) = happyShift action_54
action_276 (95) = happyShift action_55
action_276 (97) = happyShift action_56
action_276 (98) = happyShift action_57
action_276 (100) = happyShift action_101
action_276 (101) = happyShift action_102
action_276 (17) = happyGoto action_77
action_276 (20) = happyGoto action_78
action_276 (21) = happyGoto action_79
action_276 (23) = happyGoto action_80
action_276 (25) = happyGoto action_81
action_276 (27) = happyGoto action_82
action_276 (28) = happyGoto action_310
action_276 (29) = happyGoto action_104
action_276 (30) = happyGoto action_105
action_276 (31) = happyGoto action_84
action_276 (32) = happyGoto action_85
action_276 (33) = happyGoto action_86
action_276 _ = happyFail

action_277 (51) = happyShift action_38
action_277 (52) = happyShift action_87
action_277 (55) = happyShift action_88
action_277 (61) = happyShift action_89
action_277 (63) = happyShift action_90
action_277 (71) = happyShift action_91
action_277 (72) = happyShift action_92
action_277 (73) = happyShift action_93
action_277 (74) = happyShift action_94
action_277 (75) = happyShift action_95
action_277 (76) = happyShift action_96
action_277 (77) = happyShift action_97
action_277 (81) = happyShift action_98
action_277 (83) = happyShift action_99
action_277 (85) = happyShift action_100
action_277 (89) = happyShift action_52
action_277 (93) = happyShift action_53
action_277 (94) = happyShift action_54
action_277 (95) = happyShift action_55
action_277 (97) = happyShift action_56
action_277 (98) = happyShift action_57
action_277 (100) = happyShift action_101
action_277 (101) = happyShift action_102
action_277 (17) = happyGoto action_77
action_277 (20) = happyGoto action_78
action_277 (21) = happyGoto action_79
action_277 (23) = happyGoto action_80
action_277 (25) = happyGoto action_81
action_277 (27) = happyGoto action_82
action_277 (30) = happyGoto action_309
action_277 (31) = happyGoto action_84
action_277 (32) = happyGoto action_85
action_277 (33) = happyGoto action_86
action_277 _ = happyFail

action_278 _ = happyReduce_90

action_279 _ = happyReduce_51

action_280 _ = happyReduce_54

action_281 _ = happyReduce_56

action_282 _ = happyReduce_50

action_283 _ = happyReduce_53

action_284 _ = happyReduce_55

action_285 _ = happyReduce_129

action_286 (51) = happyShift action_38
action_286 (75) = happyShift action_211
action_286 (83) = happyShift action_212
action_286 (89) = happyShift action_213
action_286 (17) = happyGoto action_241
action_286 (43) = happyGoto action_242
action_286 (44) = happyGoto action_308
action_286 (45) = happyGoto action_244
action_286 (46) = happyGoto action_245
action_286 _ = happyFail

action_287 _ = happyReduce_117

action_288 _ = happyReduce_128

action_289 (51) = happyShift action_38
action_289 (17) = happyGoto action_307
action_289 _ = happyFail

action_290 (51) = happyShift action_38
action_290 (17) = happyGoto action_306
action_290 _ = happyFail

action_291 (80) = happyShift action_305
action_291 (87) = happyShift action_295
action_291 _ = happyFail

action_292 (80) = happyShift action_304
action_292 (87) = happyShift action_295
action_292 _ = happyFail

action_293 (57) = happyShift action_300
action_293 (16) = happyGoto action_303
action_293 _ = happyReduce_31

action_294 (57) = happyShift action_300
action_294 (16) = happyGoto action_302
action_294 _ = happyReduce_31

action_295 (51) = happyShift action_38
action_295 (17) = happyGoto action_36
action_295 (36) = happyGoto action_228
action_295 (37) = happyGoto action_301
action_295 _ = happyReduce_103

action_296 _ = happyReduce_138

action_297 (57) = happyShift action_300
action_297 (16) = happyGoto action_299
action_297 _ = happyReduce_31

action_298 _ = happyReduce_137

action_299 _ = happyReduce_21

action_300 (51) = happyShift action_38
action_300 (17) = happyGoto action_116
action_300 (19) = happyGoto action_320
action_300 _ = happyFail

action_301 _ = happyReduce_102

action_302 _ = happyReduce_23

action_303 _ = happyReduce_25

action_304 (57) = happyShift action_300
action_304 (16) = happyGoto action_319
action_304 _ = happyReduce_31

action_305 (57) = happyShift action_300
action_305 (16) = happyGoto action_318
action_305 _ = happyReduce_31

action_306 _ = happyReduce_125

action_307 _ = happyReduce_126

action_308 _ = happyReduce_122

action_309 (84) = happyShift action_317
action_309 _ = happyFail

action_310 _ = happyReduce_72

action_311 _ = happyReduce_59

action_312 _ = happyReduce_58

action_313 (87) = happyShift action_316
action_313 _ = happyReduce_107

action_314 _ = happyReduce_108

action_315 (99) = happyReduce_52
action_315 _ = happyReduce_52

action_316 (51) = happyShift action_38
action_316 (75) = happyShift action_211
action_316 (83) = happyShift action_212
action_316 (89) = happyShift action_213
action_316 (17) = happyGoto action_209
action_316 (39) = happyGoto action_321
action_316 (43) = happyGoto action_263
action_316 _ = happyReduce_110

action_317 _ = happyReduce_60

action_318 _ = happyReduce_22

action_319 _ = happyReduce_24

action_320 _ = happyReduce_32

action_321 _ = happyReduce_106

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (reverse happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn4
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  6 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (C.OverrideDecl Impredicative [happy_var_2]
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 6 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.OverrideDecl Impredicative happy_var_3
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_2  6 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (C.OverrideDecl Fail [happy_var_2]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 6 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.OverrideDecl Fail happy_var_3
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  6 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (C.OverrideDecl Check [happy_var_2]
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 6 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.OverrideDecl Check happy_var_3
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  6 happyReduction_19
happyReduction_19 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (C.OverrideDecl TrustMe [happy_var_2]
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 6 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.OverrideDecl TrustMe happy_var_3
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 9 7 happyReduction_21
happyReduction_21 ((HappyAbsSyn16  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.DataDecl happy_var_2 A.NotSized A.Ind happy_var_3 happy_var_5 (reverse happy_var_7) happy_var_9
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 10 8 happyReduction_22
happyReduction_22 ((HappyAbsSyn16  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_4) `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.DataDecl happy_var_3 A.Sized A.Ind happy_var_4 happy_var_6 (reverse happy_var_8) happy_var_10
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 9 9 happyReduction_23
happyReduction_23 ((HappyAbsSyn16  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.DataDecl happy_var_2 A.NotSized A.CoInd happy_var_3 happy_var_5 (reverse happy_var_7) happy_var_9
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 10 10 happyReduction_24
happyReduction_24 ((HappyAbsSyn16  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_4) `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.DataDecl happy_var_3 A.Sized A.CoInd happy_var_4 happy_var_6 (reverse happy_var_8) happy_var_10
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 9 11 happyReduction_25
happyReduction_25 ((HappyAbsSyn16  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.RecordDecl happy_var_2 happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 12 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn39  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.FunDecl A.Ind happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 5 13 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn39  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.FunDecl A.CoInd happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 4 14 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.MutualDecl (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 4 15 happyReduction_29
happyReduction_29 ((HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.LetDecl False happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 5 15 happyReduction_30
happyReduction_30 ((HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.LetDecl True happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_0  16 happyReduction_31
happyReduction_31  =  HappyAbsSyn16
		 ([]
	)

happyReduce_32 = happySpecReduce_2  16 happyReduction_32
happyReduction_32 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 (HappyTerminal (T.Id happy_var_1 _))
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  18 happyReduction_35
happyReduction_35 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 : happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  19 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  19 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 : happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  20 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn20
		 (SPos
	)

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn20
		 (Pos
	)

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn20
		 (Neg
	)

happyReduce_41 = happySpecReduce_1  20 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn20
		 (Const
	)

happyReduce_42 = happySpecReduce_1  20 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn20
		 (Param
	)

happyReduce_43 = happySpecReduce_1  20 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn20
		 (Rec
	)

happyReduce_44 = happySpecReduce_2  21 happyReduction_44
happyReduction_44 (HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (A.Measure happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  22 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  22 happyReduction_46
happyReduction_46 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  23 happyReduction_47
happyReduction_47 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn23
		 (A.Bound A.Lt happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  23 happyReduction_48
happyReduction_48 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn23
		 (A.Bound A.Le happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  24 happyReduction_49
happyReduction_49 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn16
		 (map (\ (C.Ident x) -> x) happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happyReduce 5 25 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind (Dec Default) {- A.defaultDec -} happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 5 25 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind A.irrelevantDec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_52 = happyReduce 6 25 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind (Dec happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_53 = happyReduce 5 25 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBounded A.defaultDec    happy_var_2 A.Lt happy_var_4
	) `HappyStk` happyRest

happyReduce_54 = happyReduce 5 25 happyReduction_54
happyReduction_54 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBounded A.irrelevantDec happy_var_2 A.Lt happy_var_4
	) `HappyStk` happyRest

happyReduce_55 = happyReduce 5 25 happyReduction_55
happyReduction_55 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBounded A.defaultDec    happy_var_2 A.Le happy_var_4
	) `HappyStk` happyRest

happyReduce_56 = happyReduce 5 25 happyReduction_56
happyReduction_56 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBounded A.irrelevantDec happy_var_2 A.Le happy_var_4
	) `HappyStk` happyRest

happyReduce_57 = happySpecReduce_3  26 happyReduction_57
happyReduction_57 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn25
		 (C.TBind A.defaultDec [happy_var_1] happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happyReduce 5 26 happyReduction_58
happyReduction_58 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind A.defaultDec [happy_var_2] happy_var_4
	) `HappyStk` happyRest

happyReduce_59 = happyReduce 5 26 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind A.irrelevantDec [happy_var_2] happy_var_4
	) `HappyStk` happyRest

happyReduce_60 = happyReduce 6 26 happyReduction_60
happyReduction_60 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind (Dec happy_var_1) [happy_var_3] happy_var_5
	) `HappyStk` happyRest

happyReduce_61 = happySpecReduce_1  27 happyReduction_61
happyReduction_61 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn25
		 (C.TBind (Dec Default) {- A.defaultDec -} [] happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  27 happyReduction_62
happyReduction_62 _
	(HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (C.TBind A.irrelevantDec [] happy_var_2
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  27 happyReduction_63
happyReduction_63 (HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn25
		 (C.TBind (Dec happy_var_1) [] happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  27 happyReduction_64
happyReduction_64 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  27 happyReduction_65
happyReduction_65 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn25
		 (C.TMeasure happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  27 happyReduction_66
happyReduction_66 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn25
		 (C.TBound happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  28 happyReduction_67
happyReduction_67 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn28
		 (foldr1 C.Pair happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  29 happyReduction_68
happyReduction_68 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  29 happyReduction_69
happyReduction_69 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  30 happyReduction_70
happyReduction_70 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn28
		 (C.Quant A.Pi happy_var_1 happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happyReduce 4 30 happyReduction_71
happyReduction_71 ((HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (foldr C.Lam happy_var_4 happy_var_2
	) `HappyStk` happyRest

happyReduce_72 = happyReduce 6 30 happyReduction_72
happyReduction_72 ((HappyAbsSyn28  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (C.LLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_73 = happyReduce 5 30 happyReduction_73
happyReduction_73 (_ `HappyStk`
	(HappyAbsSyn39  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (C.Case happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_74 = happySpecReduce_1  30 happyReduction_74
happyReduction_74 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  30 happyReduction_75
happyReduction_75 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 (C.Plus happy_var_1 happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  31 happyReduction_76
happyReduction_76 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn28
		 (let (f : args) = reverse happy_var_1 in
                if null args then f else C.App f args
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_2  31 happyReduction_77
happyReduction_77 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (C.CoSet happy_var_2
	)
happyReduction_77 _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  31 happyReduction_78
happyReduction_78 _
	 =  HappyAbsSyn28
		 (C.Set C.Zero
	)

happyReduce_79 = happySpecReduce_2  31 happyReduction_79
happyReduction_79 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (C.Set happy_var_2
	)
happyReduction_79 _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  31 happyReduction_80
happyReduction_80 (HappyAbsSyn28  happy_var_3)
	_
	(HappyTerminal (T.Number happy_var_1 _))
	 =  HappyAbsSyn28
		 (let n = read happy_var_1 in
                                            if n==0 then C.Zero else
                                            iterate (C.Plus happy_var_3) happy_var_3 !! (n-1)
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  31 happyReduction_81
happyReduction_81 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn28
		 (C.Quant A.Sigma happy_var_1 happy_var_3
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  32 happyReduction_82
happyReduction_82 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_2  32 happyReduction_83
happyReduction_83 (HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_1
	)
happyReduction_83 _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  32 happyReduction_84
happyReduction_84 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (C.Proj happy_var_3 : happy_var_1
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_2  32 happyReduction_85
happyReduction_85 _
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (C.Set C.Zero : happy_var_1
	)
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  33 happyReduction_86
happyReduction_86 _
	 =  HappyAbsSyn28
		 (C.Size
	)

happyReduce_87 = happySpecReduce_1  33 happyReduction_87
happyReduction_87 _
	 =  HappyAbsSyn28
		 (C.Max
	)

happyReduce_88 = happySpecReduce_1  33 happyReduction_88
happyReduction_88 _
	 =  HappyAbsSyn28
		 (C.Infty
	)

happyReduce_89 = happySpecReduce_1  33 happyReduction_89
happyReduction_89 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn28
		 (C.Ident happy_var_1
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happyReduce 5 33 happyReduction_90
happyReduction_90 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (C.Sing happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_91 = happySpecReduce_3  33 happyReduction_91
happyReduction_91 _
	(HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (happy_var_2
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  33 happyReduction_92
happyReduction_92 _
	 =  HappyAbsSyn28
		 (C.Unknown
	)

happyReduce_93 = happySpecReduce_2  33 happyReduction_93
happyReduction_93 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (C.Succ happy_var_2
	)
happyReduction_93 _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  33 happyReduction_94
happyReduction_94 (HappyTerminal (T.Number happy_var_1 _))
	 =  HappyAbsSyn28
		 (iterate C.Succ C.Zero !! (read happy_var_1)
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happyReduce 4 33 happyReduction_95
happyReduction_95 (_ `HappyStk`
	(HappyAbsSyn34  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (C.Record happy_var_3
	) `HappyStk` happyRest

happyReduce_96 = happySpecReduce_3  34 happyReduction_96
happyReduction_96 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1 : happy_var_3
	)
happyReduction_96 _ _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  34 happyReduction_97
happyReduction_97 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn34
		 ([happy_var_1]
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_0  34 happyReduction_98
happyReduction_98  =  HappyAbsSyn34
		 ([]
	)

happyReduce_99 = happySpecReduce_3  35 happyReduction_99
happyReduction_99 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn35
		 ((happy_var_1,happy_var_3)
	)
happyReduction_99 _ _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_3  36 happyReduction_100
happyReduction_100 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn36
		 (C.TypeSig happy_var_1 happy_var_3
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1  37 happyReduction_101
happyReduction_101 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_3  38 happyReduction_102
happyReduction_102 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_3 : happy_var_1
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_2  38 happyReduction_103
happyReduction_103 _
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_1
	)
happyReduction_103 _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_1  38 happyReduction_104
happyReduction_104 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn38
		 ([happy_var_1]
	)
happyReduction_104 _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_0  38 happyReduction_105
happyReduction_105  =  HappyAbsSyn38
		 ([]
	)

happyReduce_106 = happyReduce 5 39 happyReduction_106
happyReduction_106 ((HappyAbsSyn39  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 ((C.Clause Nothing [happy_var_1] (Just happy_var_3)) : happy_var_5
	) `HappyStk` happyRest

happyReduce_107 = happySpecReduce_3  39 happyReduction_107
happyReduction_107 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn39
		 ((C.Clause Nothing [happy_var_1] (Just happy_var_3)) : []
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_3  39 happyReduction_108
happyReduction_108 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn39
		 ((C.Clause Nothing [happy_var_1] Nothing) : happy_var_3
	)
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_1  39 happyReduction_109
happyReduction_109 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn39
		 ((C.Clause Nothing [happy_var_1] Nothing) : []
	)
happyReduction_109 _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_0  39 happyReduction_110
happyReduction_110  =  HappyAbsSyn39
		 ([]
	)

happyReduce_111 = happyReduce 4 40 happyReduction_111
happyReduction_111 ((HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_2) `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn40
		 (C.Clause (Just happy_var_1) happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_112 = happySpecReduce_2  40 happyReduction_112
happyReduction_112 (HappyAbsSyn41  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn40
		 (C.Clause (Just happy_var_1) happy_var_2 Nothing
	)
happyReduction_112 _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_1  41 happyReduction_113
happyReduction_113 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (reverse happy_var_1
	)
happyReduction_113 _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_0  42 happyReduction_114
happyReduction_114  =  HappyAbsSyn41
		 ([]
	)

happyReduce_115 = happySpecReduce_2  42 happyReduction_115
happyReduction_115 (HappyAbsSyn43  happy_var_2)
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_2 : happy_var_1
	)
happyReduction_115 _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_2  43 happyReduction_116
happyReduction_116 _
	_
	 =  HappyAbsSyn43
		 (C.AbsurdP
	)

happyReduce_117 = happySpecReduce_3  43 happyReduction_117
happyReduction_117 _
	(HappyAbsSyn43  happy_var_2)
	_
	 =  HappyAbsSyn43
		 (happy_var_2
	)
happyReduction_117 _ _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  43 happyReduction_118
happyReduction_118 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn43
		 (C.IdentP happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_2  43 happyReduction_119
happyReduction_119 (HappyAbsSyn43  happy_var_2)
	_
	 =  HappyAbsSyn43
		 (C.SuccP happy_var_2
	)
happyReduction_119 _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_2  43 happyReduction_120
happyReduction_120 _
	_
	 =  HappyAbsSyn43
		 (C.DotP (C.Set C.Zero)
	)

happyReduce_121 = happySpecReduce_2  43 happyReduction_121
happyReduction_121 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn43
		 (C.DotP happy_var_2
	)
happyReduction_121 _ _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_3  44 happyReduction_122
happyReduction_122 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (C.PairP happy_var_1 happy_var_3
	)
happyReduction_122 _ _ _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  44 happyReduction_123
happyReduction_123 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_1  45 happyReduction_124
happyReduction_124 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn43
		 (let (c, ps) = happy_var_1 in C.ConP c (reverse ps)
	)
happyReduction_124 _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_3  45 happyReduction_125
happyReduction_125 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn43
		 (C.SizeP happy_var_1 happy_var_3
	)
happyReduction_125 _ _ _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_3  45 happyReduction_126
happyReduction_126 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn43
		 (C.SizeP happy_var_3 happy_var_1
	)
happyReduction_126 _ _ _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_1  45 happyReduction_127
happyReduction_127 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1
	)
happyReduction_127 _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_2  46 happyReduction_128
happyReduction_128 (HappyAbsSyn43  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn46
		 ((happy_var_1, [happy_var_2])
	)
happyReduction_128 _ _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_2  46 happyReduction_129
happyReduction_129 (HappyAbsSyn43  happy_var_2)
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn46
		 (let (c, ps) = happy_var_1 in (c, happy_var_2 : ps)
	)
happyReduction_129 _ _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_1  47 happyReduction_130
happyReduction_130 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn39
		 (reverse happy_var_1
	)
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_3  48 happyReduction_131
happyReduction_131 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_3 : happy_var_1
	)
happyReduction_131 _ _ _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_2  48 happyReduction_132
happyReduction_132 _
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_132 _ _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_1  48 happyReduction_133
happyReduction_133 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn48
		 ([happy_var_1]
	)
happyReduction_133 _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_0  48 happyReduction_134
happyReduction_134  =  HappyAbsSyn48
		 ([]
	)

happyReduce_135 = happyReduce 5 49 happyReduction_135
happyReduction_135 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind A.paramDec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_136 = happyReduce 5 49 happyReduction_136
happyReduction_136 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind A.irrelevantDec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_137 = happyReduce 6 49 happyReduction_137
happyReduction_137 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind (Dec happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_138 = happyReduce 6 49 happyReduction_138
happyReduction_138 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind (Dec SPos) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_139 = happyReduce 4 49 happyReduction_139
happyReduction_139 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TSized happy_var_3
	) `HappyStk` happyRest

happyReduce_140 = happySpecReduce_0  50 happyReduction_140
happyReduction_140  =  HappyAbsSyn50
		 ([]
	)

happyReduce_141 = happySpecReduce_2  50 happyReduction_141
happyReduction_141 (HappyAbsSyn50  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1 : happy_var_2
	)
happyReduction_141 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 102 102 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T.Id happy_dollar_dollar _ -> cont 51;
	T.Number happy_dollar_dollar _ -> cont 52;
	T.Data _ -> cont 53;
	T.CoData _ -> cont 54;
	T.Record _ -> cont 55;
	T.Sized _ -> cont 56;
	T.Fields _ -> cont 57;
	T.Mutual _ -> cont 58;
	T.Fun _ -> cont 59;
	T.CoFun _ -> cont 60;
	T.Case _ -> cont 61;
	T.Def _ -> cont 62;
	T.Let _ -> cont 63;
	T.In _ -> cont 64;
	T.Eval _ -> cont 65;
	T.Fail _ -> cont 66;
	T.Check _ -> cont 67;
	T.TrustMe _ -> cont 68;
	T.Impredicative _ -> cont 69;
	T.Type _ -> cont 70;
	T.Set _ -> cont 71;
	T.CoSet _ -> cont 72;
	T.Size _ -> cont 73;
	T.Infty _ -> cont 74;
	T.Succ _ -> cont 75;
	T.Max _ -> cont 76;
	T.AngleOpen _ -> cont 77;
	T.AngleClose _ -> cont 78;
	T.BrOpen _ -> cont 79;
	T.BrClose _ -> cont 80;
	T.BracketOpen _ -> cont 81;
	T.BracketClose _ -> cont 82;
	T.PrOpen _ -> cont 83;
	T.PrClose _ -> cont 84;
	T.Bar _ -> cont 85;
	T.Comma _ -> cont 86;
	T.Sem _ -> cont 87;
	T.Col _ -> cont 88;
	T.Dot _ -> cont 89;
	T.Arrow _ -> cont 90;
	T.Leq _ -> cont 91;
	T.Eq _ -> cont 92;
	T.PlusPlus _ -> cont 93;
	T.Plus _ -> cont 94;
	T.Minus _ -> cont 95;
	T.Slash _ -> cont 96;
	T.Times _ -> cont 97;
	T.Hat _ -> cont 98;
	T.Amp _ -> cont 99;
	T.Lam _ -> cont 100;
	T.Underscore _ -> cont 101;
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

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

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

{-# LINE 148 "templates/GenericTemplate.hs" #-}

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
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
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
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
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
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 311 "templates/GenericTemplate.hs" #-}
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
