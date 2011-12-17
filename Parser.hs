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
	| HappyAbsSyn24 (C.TBind)
	| HappyAbsSyn27 (C.Expr)
	| HappyAbsSyn32 ([([Name],C.Expr)])
	| HappyAbsSyn33 (([Name],C.Expr))
	| HappyAbsSyn34 (C.TypeSig)
	| HappyAbsSyn35 (C.Constructor)
	| HappyAbsSyn36 ([C.Constructor ])
	| HappyAbsSyn37 ([C.Clause])
	| HappyAbsSyn38 (C.Clause)
	| HappyAbsSyn39 ([C.Pattern])
	| HappyAbsSyn41 (C.Pattern)
	| HappyAbsSyn44 ((Name, [C.Pattern]))
	| HappyAbsSyn46 ([C.Clause ])
	| HappyAbsSyn48 (C.Telescope)

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
 action_319 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_139 :: () => ({-HappyReduction (HappyIdentity) = -}
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

action_2 (51) = happyShift action_14
action_2 (52) = happyShift action_15
action_2 (53) = happyShift action_16
action_2 (54) = happyShift action_17
action_2 (56) = happyShift action_18
action_2 (57) = happyShift action_19
action_2 (58) = happyShift action_20
action_2 (61) = happyShift action_21
action_2 (63) = happyShift action_22
action_2 (64) = happyShift action_23
action_2 (65) = happyShift action_24
action_2 (66) = happyShift action_25
action_2 (67) = happyShift action_26
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

action_3 (100) = happyAccept
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

action_14 (49) = happyShift action_38
action_14 (17) = happyGoto action_46
action_14 _ = happyFail

action_15 (49) = happyShift action_38
action_15 (17) = happyGoto action_45
action_15 _ = happyFail

action_16 (49) = happyShift action_38
action_16 (17) = happyGoto action_44
action_16 _ = happyFail

action_17 (51) = happyShift action_42
action_17 (52) = happyShift action_43
action_17 _ = happyFail

action_18 (77) = happyShift action_41
action_18 _ = happyFail

action_19 (49) = happyShift action_38
action_19 (17) = happyGoto action_36
action_19 (34) = happyGoto action_40
action_19 _ = happyFail

action_20 (49) = happyShift action_38
action_20 (17) = happyGoto action_36
action_20 (34) = happyGoto action_39
action_20 _ = happyFail

action_21 (49) = happyShift action_38
action_21 (17) = happyGoto action_36
action_21 (34) = happyGoto action_37
action_21 _ = happyFail

action_22 (61) = happyShift action_35
action_22 _ = happyFail

action_23 (51) = happyShift action_14
action_23 (52) = happyShift action_15
action_23 (53) = happyShift action_16
action_23 (54) = happyShift action_17
action_23 (56) = happyShift action_18
action_23 (57) = happyShift action_19
action_23 (58) = happyShift action_20
action_23 (61) = happyShift action_21
action_23 (63) = happyShift action_22
action_23 (64) = happyShift action_23
action_23 (65) = happyShift action_24
action_23 (66) = happyShift action_25
action_23 (67) = happyShift action_26
action_23 (77) = happyShift action_34
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

action_24 (51) = happyShift action_14
action_24 (52) = happyShift action_15
action_24 (53) = happyShift action_16
action_24 (54) = happyShift action_17
action_24 (56) = happyShift action_18
action_24 (57) = happyShift action_19
action_24 (58) = happyShift action_20
action_24 (61) = happyShift action_21
action_24 (63) = happyShift action_22
action_24 (64) = happyShift action_23
action_24 (65) = happyShift action_24
action_24 (66) = happyShift action_25
action_24 (67) = happyShift action_26
action_24 (77) = happyShift action_32
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

action_25 (51) = happyShift action_14
action_25 (52) = happyShift action_15
action_25 (53) = happyShift action_16
action_25 (54) = happyShift action_17
action_25 (56) = happyShift action_18
action_25 (57) = happyShift action_19
action_25 (58) = happyShift action_20
action_25 (61) = happyShift action_21
action_25 (63) = happyShift action_22
action_25 (64) = happyShift action_23
action_25 (65) = happyShift action_24
action_25 (66) = happyShift action_25
action_25 (67) = happyShift action_26
action_25 (77) = happyShift action_30
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

action_26 (51) = happyShift action_14
action_26 (52) = happyShift action_15
action_26 (53) = happyShift action_16
action_26 (54) = happyShift action_17
action_26 (56) = happyShift action_18
action_26 (57) = happyShift action_19
action_26 (58) = happyShift action_20
action_26 (61) = happyShift action_21
action_26 (63) = happyShift action_22
action_26 (64) = happyShift action_23
action_26 (65) = happyShift action_24
action_26 (66) = happyShift action_25
action_26 (67) = happyShift action_26
action_26 (77) = happyShift action_28
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

action_35 (49) = happyShift action_38
action_35 (17) = happyGoto action_36
action_35 (34) = happyGoto action_67
action_35 _ = happyFail

action_36 (86) = happyShift action_66
action_36 _ = happyFail

action_37 (90) = happyShift action_65
action_37 _ = happyFail

action_38 _ = happyReduce_33

action_39 (77) = happyShift action_64
action_39 _ = happyFail

action_40 (77) = happyShift action_63
action_40 _ = happyFail

action_41 (5) = happyGoto action_62
action_41 _ = happyReduce_2

action_42 (49) = happyShift action_38
action_42 (17) = happyGoto action_61
action_42 _ = happyFail

action_43 (49) = happyShift action_38
action_43 (17) = happyGoto action_60
action_43 _ = happyFail

action_44 (79) = happyShift action_50
action_44 (81) = happyShift action_51
action_44 (87) = happyShift action_52
action_44 (91) = happyShift action_53
action_44 (92) = happyShift action_54
action_44 (93) = happyShift action_55
action_44 (95) = happyShift action_56
action_44 (96) = happyShift action_57
action_44 (20) = happyGoto action_47
action_44 (47) = happyGoto action_48
action_44 (48) = happyGoto action_59
action_44 _ = happyReduce_138

action_45 (79) = happyShift action_50
action_45 (81) = happyShift action_51
action_45 (87) = happyShift action_52
action_45 (91) = happyShift action_53
action_45 (92) = happyShift action_54
action_45 (93) = happyShift action_55
action_45 (95) = happyShift action_56
action_45 (96) = happyShift action_57
action_45 (20) = happyGoto action_47
action_45 (47) = happyGoto action_48
action_45 (48) = happyGoto action_58
action_45 _ = happyReduce_138

action_46 (79) = happyShift action_50
action_46 (81) = happyShift action_51
action_46 (87) = happyShift action_52
action_46 (91) = happyShift action_53
action_46 (92) = happyShift action_54
action_46 (93) = happyShift action_55
action_46 (95) = happyShift action_56
action_46 (96) = happyShift action_57
action_46 (20) = happyGoto action_47
action_46 (47) = happyGoto action_48
action_46 (48) = happyGoto action_49
action_46 _ = happyReduce_138

action_47 (81) = happyShift action_122
action_47 _ = happyFail

action_48 (79) = happyShift action_50
action_48 (81) = happyShift action_51
action_48 (87) = happyShift action_52
action_48 (91) = happyShift action_53
action_48 (92) = happyShift action_54
action_48 (93) = happyShift action_55
action_48 (95) = happyShift action_56
action_48 (96) = happyShift action_57
action_48 (20) = happyGoto action_47
action_48 (47) = happyGoto action_48
action_48 (48) = happyGoto action_121
action_48 _ = happyReduce_138

action_49 (86) = happyShift action_120
action_49 _ = happyFail

action_50 (49) = happyShift action_38
action_50 (17) = happyGoto action_115
action_50 (19) = happyGoto action_119
action_50 _ = happyFail

action_51 (49) = happyShift action_38
action_51 (54) = happyShift action_117
action_51 (92) = happyShift action_118
action_51 (17) = happyGoto action_115
action_51 (19) = happyGoto action_116
action_51 _ = happyFail

action_52 _ = happyReduce_41

action_53 _ = happyReduce_38

action_54 _ = happyReduce_39

action_55 _ = happyReduce_40

action_56 _ = happyReduce_43

action_57 _ = happyReduce_42

action_58 (86) = happyShift action_114
action_58 _ = happyFail

action_59 (86) = happyShift action_113
action_59 _ = happyFail

action_60 (79) = happyShift action_50
action_60 (81) = happyShift action_51
action_60 (87) = happyShift action_52
action_60 (91) = happyShift action_53
action_60 (92) = happyShift action_54
action_60 (93) = happyShift action_55
action_60 (95) = happyShift action_56
action_60 (96) = happyShift action_57
action_60 (20) = happyGoto action_47
action_60 (47) = happyGoto action_48
action_60 (48) = happyGoto action_112
action_60 _ = happyReduce_138

action_61 (79) = happyShift action_50
action_61 (81) = happyShift action_51
action_61 (87) = happyShift action_52
action_61 (91) = happyShift action_53
action_61 (92) = happyShift action_54
action_61 (93) = happyShift action_55
action_61 (95) = happyShift action_56
action_61 (96) = happyShift action_57
action_61 (20) = happyGoto action_47
action_61 (47) = happyGoto action_48
action_61 (48) = happyGoto action_111
action_61 _ = happyReduce_138

action_62 (51) = happyShift action_14
action_62 (52) = happyShift action_15
action_62 (53) = happyShift action_16
action_62 (54) = happyShift action_17
action_62 (56) = happyShift action_18
action_62 (57) = happyShift action_19
action_62 (58) = happyShift action_20
action_62 (61) = happyShift action_21
action_62 (63) = happyShift action_22
action_62 (64) = happyShift action_23
action_62 (65) = happyShift action_24
action_62 (66) = happyShift action_25
action_62 (67) = happyShift action_26
action_62 (78) = happyShift action_110
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

action_63 (49) = happyShift action_38
action_63 (17) = happyGoto action_105
action_63 (38) = happyGoto action_106
action_63 (45) = happyGoto action_109
action_63 (46) = happyGoto action_108
action_63 _ = happyReduce_132

action_64 (49) = happyShift action_38
action_64 (17) = happyGoto action_105
action_64 (38) = happyGoto action_106
action_64 (45) = happyGoto action_107
action_64 (46) = happyGoto action_108
action_64 _ = happyReduce_132

action_65 (49) = happyShift action_38
action_65 (50) = happyShift action_87
action_65 (53) = happyShift action_88
action_65 (59) = happyShift action_89
action_65 (61) = happyShift action_90
action_65 (69) = happyShift action_91
action_65 (70) = happyShift action_92
action_65 (71) = happyShift action_93
action_65 (72) = happyShift action_94
action_65 (73) = happyShift action_95
action_65 (74) = happyShift action_96
action_65 (75) = happyShift action_97
action_65 (79) = happyShift action_98
action_65 (81) = happyShift action_99
action_65 (83) = happyShift action_100
action_65 (87) = happyShift action_52
action_65 (91) = happyShift action_53
action_65 (92) = happyShift action_54
action_65 (93) = happyShift action_55
action_65 (95) = happyShift action_56
action_65 (96) = happyShift action_57
action_65 (98) = happyShift action_101
action_65 (99) = happyShift action_102
action_65 (17) = happyGoto action_77
action_65 (20) = happyGoto action_78
action_65 (21) = happyGoto action_79
action_65 (23) = happyGoto action_80
action_65 (24) = happyGoto action_81
action_65 (26) = happyGoto action_82
action_65 (27) = happyGoto action_103
action_65 (28) = happyGoto action_104
action_65 (29) = happyGoto action_84
action_65 (30) = happyGoto action_85
action_65 (31) = happyGoto action_86
action_65 _ = happyFail

action_66 (49) = happyShift action_38
action_66 (50) = happyShift action_87
action_66 (53) = happyShift action_88
action_66 (59) = happyShift action_89
action_66 (61) = happyShift action_90
action_66 (69) = happyShift action_91
action_66 (70) = happyShift action_92
action_66 (71) = happyShift action_93
action_66 (72) = happyShift action_94
action_66 (73) = happyShift action_95
action_66 (74) = happyShift action_96
action_66 (75) = happyShift action_97
action_66 (79) = happyShift action_98
action_66 (81) = happyShift action_99
action_66 (83) = happyShift action_100
action_66 (87) = happyShift action_52
action_66 (91) = happyShift action_53
action_66 (92) = happyShift action_54
action_66 (93) = happyShift action_55
action_66 (95) = happyShift action_56
action_66 (96) = happyShift action_57
action_66 (98) = happyShift action_101
action_66 (99) = happyShift action_102
action_66 (17) = happyGoto action_77
action_66 (20) = happyGoto action_78
action_66 (21) = happyGoto action_79
action_66 (23) = happyGoto action_80
action_66 (24) = happyGoto action_81
action_66 (26) = happyGoto action_82
action_66 (28) = happyGoto action_83
action_66 (29) = happyGoto action_84
action_66 (30) = happyGoto action_85
action_66 (31) = happyGoto action_86
action_66 _ = happyFail

action_67 (90) = happyShift action_76
action_67 _ = happyFail

action_68 (51) = happyShift action_14
action_68 (52) = happyShift action_15
action_68 (53) = happyShift action_16
action_68 (54) = happyShift action_17
action_68 (56) = happyShift action_18
action_68 (57) = happyShift action_19
action_68 (58) = happyShift action_20
action_68 (61) = happyShift action_21
action_68 (63) = happyShift action_22
action_68 (64) = happyShift action_23
action_68 (65) = happyShift action_24
action_68 (66) = happyShift action_25
action_68 (67) = happyShift action_26
action_68 (78) = happyShift action_75
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

action_69 (51) = happyShift action_14
action_69 (52) = happyShift action_15
action_69 (53) = happyShift action_16
action_69 (54) = happyShift action_17
action_69 (56) = happyShift action_18
action_69 (57) = happyShift action_19
action_69 (58) = happyShift action_20
action_69 (61) = happyShift action_21
action_69 (63) = happyShift action_22
action_69 (64) = happyShift action_23
action_69 (65) = happyShift action_24
action_69 (66) = happyShift action_25
action_69 (67) = happyShift action_26
action_69 (78) = happyShift action_74
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

action_70 (51) = happyShift action_14
action_70 (52) = happyShift action_15
action_70 (53) = happyShift action_16
action_70 (54) = happyShift action_17
action_70 (56) = happyShift action_18
action_70 (57) = happyShift action_19
action_70 (58) = happyShift action_20
action_70 (61) = happyShift action_21
action_70 (63) = happyShift action_22
action_70 (64) = happyShift action_23
action_70 (65) = happyShift action_24
action_70 (66) = happyShift action_25
action_70 (67) = happyShift action_26
action_70 (78) = happyShift action_73
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

action_71 (51) = happyShift action_14
action_71 (52) = happyShift action_15
action_71 (53) = happyShift action_16
action_71 (54) = happyShift action_17
action_71 (56) = happyShift action_18
action_71 (57) = happyShift action_19
action_71 (58) = happyShift action_20
action_71 (61) = happyShift action_21
action_71 (63) = happyShift action_22
action_71 (64) = happyShift action_23
action_71 (65) = happyShift action_24
action_71 (66) = happyShift action_25
action_71 (67) = happyShift action_26
action_71 (78) = happyShift action_72
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

action_76 (49) = happyShift action_38
action_76 (50) = happyShift action_87
action_76 (53) = happyShift action_88
action_76 (59) = happyShift action_89
action_76 (61) = happyShift action_90
action_76 (69) = happyShift action_91
action_76 (70) = happyShift action_92
action_76 (71) = happyShift action_93
action_76 (72) = happyShift action_94
action_76 (73) = happyShift action_95
action_76 (74) = happyShift action_96
action_76 (75) = happyShift action_97
action_76 (79) = happyShift action_98
action_76 (81) = happyShift action_99
action_76 (83) = happyShift action_100
action_76 (87) = happyShift action_52
action_76 (91) = happyShift action_53
action_76 (92) = happyShift action_54
action_76 (93) = happyShift action_55
action_76 (95) = happyShift action_56
action_76 (96) = happyShift action_57
action_76 (98) = happyShift action_101
action_76 (99) = happyShift action_102
action_76 (17) = happyGoto action_77
action_76 (20) = happyGoto action_78
action_76 (21) = happyGoto action_79
action_76 (23) = happyGoto action_80
action_76 (24) = happyGoto action_81
action_76 (26) = happyGoto action_82
action_76 (27) = happyGoto action_175
action_76 (28) = happyGoto action_104
action_76 (29) = happyGoto action_84
action_76 (30) = happyGoto action_85
action_76 (31) = happyGoto action_86
action_76 _ = happyFail

action_77 _ = happyReduce_87

action_78 (49) = happyShift action_38
action_78 (50) = happyShift action_87
action_78 (53) = happyShift action_88
action_78 (69) = happyShift action_91
action_78 (70) = happyShift action_92
action_78 (71) = happyShift action_93
action_78 (72) = happyShift action_94
action_78 (73) = happyShift action_95
action_78 (74) = happyShift action_96
action_78 (75) = happyShift action_97
action_78 (79) = happyShift action_98
action_78 (81) = happyShift action_174
action_78 (83) = happyShift action_100
action_78 (87) = happyShift action_52
action_78 (91) = happyShift action_53
action_78 (92) = happyShift action_54
action_78 (93) = happyShift action_55
action_78 (95) = happyShift action_56
action_78 (96) = happyShift action_57
action_78 (99) = happyShift action_102
action_78 (17) = happyGoto action_77
action_78 (20) = happyGoto action_78
action_78 (21) = happyGoto action_79
action_78 (23) = happyGoto action_80
action_78 (24) = happyGoto action_81
action_78 (26) = happyGoto action_172
action_78 (29) = happyGoto action_173
action_78 (30) = happyGoto action_85
action_78 (31) = happyGoto action_86
action_78 _ = happyFail

action_79 (75) = happyShift action_170
action_79 (89) = happyShift action_171
action_79 _ = happyReduce_64

action_80 _ = happyReduce_65

action_81 _ = happyReduce_63

action_82 (88) = happyShift action_168
action_82 (97) = happyShift action_169
action_82 _ = happyFail

action_83 _ = happyReduce_98

action_84 (88) = happyReduce_60
action_84 (92) = happyShift action_167
action_84 (97) = happyReduce_60
action_84 _ = happyReduce_72

action_85 (49) = happyShift action_38
action_85 (50) = happyShift action_152
action_85 (53) = happyShift action_88
action_85 (69) = happyShift action_165
action_85 (71) = happyShift action_93
action_85 (72) = happyShift action_94
action_85 (73) = happyShift action_95
action_85 (74) = happyShift action_96
action_85 (75) = happyShift action_97
action_85 (81) = happyShift action_153
action_85 (87) = happyShift action_166
action_85 (99) = happyShift action_102
action_85 (17) = happyGoto action_77
action_85 (31) = happyGoto action_164
action_85 _ = happyReduce_74

action_86 _ = happyReduce_80

action_87 (95) = happyShift action_163
action_87 _ = happyReduce_92

action_88 (77) = happyShift action_162
action_88 _ = happyFail

action_89 (49) = happyShift action_38
action_89 (50) = happyShift action_87
action_89 (53) = happyShift action_88
action_89 (59) = happyShift action_89
action_89 (61) = happyShift action_90
action_89 (69) = happyShift action_91
action_89 (70) = happyShift action_92
action_89 (71) = happyShift action_93
action_89 (72) = happyShift action_94
action_89 (73) = happyShift action_95
action_89 (74) = happyShift action_96
action_89 (75) = happyShift action_97
action_89 (79) = happyShift action_98
action_89 (81) = happyShift action_99
action_89 (83) = happyShift action_100
action_89 (87) = happyShift action_52
action_89 (91) = happyShift action_53
action_89 (92) = happyShift action_54
action_89 (93) = happyShift action_55
action_89 (95) = happyShift action_56
action_89 (96) = happyShift action_57
action_89 (98) = happyShift action_101
action_89 (99) = happyShift action_102
action_89 (17) = happyGoto action_77
action_89 (20) = happyGoto action_78
action_89 (21) = happyGoto action_79
action_89 (23) = happyGoto action_80
action_89 (24) = happyGoto action_81
action_89 (26) = happyGoto action_82
action_89 (27) = happyGoto action_161
action_89 (28) = happyGoto action_104
action_89 (29) = happyGoto action_84
action_89 (30) = happyGoto action_85
action_89 (31) = happyGoto action_86
action_89 _ = happyFail

action_90 (49) = happyShift action_38
action_90 (79) = happyShift action_159
action_90 (81) = happyShift action_160
action_90 (87) = happyShift action_52
action_90 (91) = happyShift action_53
action_90 (92) = happyShift action_54
action_90 (93) = happyShift action_55
action_90 (95) = happyShift action_56
action_90 (96) = happyShift action_57
action_90 (17) = happyGoto action_156
action_90 (20) = happyGoto action_157
action_90 (25) = happyGoto action_158
action_90 _ = happyFail

action_91 (49) = happyShift action_38
action_91 (50) = happyShift action_152
action_91 (53) = happyShift action_88
action_91 (71) = happyShift action_93
action_91 (72) = happyShift action_94
action_91 (73) = happyShift action_95
action_91 (74) = happyShift action_96
action_91 (75) = happyShift action_97
action_91 (81) = happyShift action_153
action_91 (99) = happyShift action_102
action_91 (17) = happyGoto action_77
action_91 (31) = happyGoto action_155
action_91 _ = happyReduce_76

action_92 (49) = happyShift action_38
action_92 (50) = happyShift action_152
action_92 (53) = happyShift action_88
action_92 (71) = happyShift action_93
action_92 (72) = happyShift action_94
action_92 (73) = happyShift action_95
action_92 (74) = happyShift action_96
action_92 (75) = happyShift action_97
action_92 (81) = happyShift action_153
action_92 (99) = happyShift action_102
action_92 (17) = happyGoto action_77
action_92 (31) = happyGoto action_154
action_92 _ = happyFail

action_93 _ = happyReduce_84

action_94 _ = happyReduce_86

action_95 (49) = happyShift action_38
action_95 (50) = happyShift action_152
action_95 (53) = happyShift action_88
action_95 (71) = happyShift action_93
action_95 (72) = happyShift action_94
action_95 (73) = happyShift action_95
action_95 (74) = happyShift action_96
action_95 (75) = happyShift action_97
action_95 (81) = happyShift action_153
action_95 (99) = happyShift action_102
action_95 (17) = happyGoto action_77
action_95 (31) = happyGoto action_151
action_95 _ = happyFail

action_96 _ = happyReduce_85

action_97 (49) = happyShift action_38
action_97 (50) = happyShift action_87
action_97 (53) = happyShift action_88
action_97 (59) = happyShift action_89
action_97 (61) = happyShift action_90
action_97 (69) = happyShift action_91
action_97 (70) = happyShift action_92
action_97 (71) = happyShift action_93
action_97 (72) = happyShift action_94
action_97 (73) = happyShift action_95
action_97 (74) = happyShift action_96
action_97 (75) = happyShift action_97
action_97 (79) = happyShift action_98
action_97 (81) = happyShift action_99
action_97 (83) = happyShift action_100
action_97 (87) = happyShift action_52
action_97 (91) = happyShift action_53
action_97 (92) = happyShift action_54
action_97 (93) = happyShift action_55
action_97 (95) = happyShift action_56
action_97 (96) = happyShift action_57
action_97 (98) = happyShift action_101
action_97 (99) = happyShift action_102
action_97 (17) = happyGoto action_77
action_97 (20) = happyGoto action_78
action_97 (21) = happyGoto action_79
action_97 (23) = happyGoto action_80
action_97 (24) = happyGoto action_81
action_97 (26) = happyGoto action_82
action_97 (27) = happyGoto action_150
action_97 (28) = happyGoto action_104
action_97 (29) = happyGoto action_84
action_97 (30) = happyGoto action_85
action_97 (31) = happyGoto action_86
action_97 _ = happyFail

action_98 (49) = happyShift action_38
action_98 (50) = happyShift action_87
action_98 (53) = happyShift action_88
action_98 (59) = happyShift action_89
action_98 (61) = happyShift action_90
action_98 (69) = happyShift action_91
action_98 (70) = happyShift action_92
action_98 (71) = happyShift action_93
action_98 (72) = happyShift action_94
action_98 (73) = happyShift action_95
action_98 (74) = happyShift action_96
action_98 (75) = happyShift action_97
action_98 (79) = happyShift action_98
action_98 (81) = happyShift action_99
action_98 (83) = happyShift action_100
action_98 (87) = happyShift action_52
action_98 (91) = happyShift action_53
action_98 (92) = happyShift action_54
action_98 (93) = happyShift action_55
action_98 (95) = happyShift action_56
action_98 (96) = happyShift action_57
action_98 (98) = happyShift action_101
action_98 (99) = happyShift action_102
action_98 (17) = happyGoto action_147
action_98 (19) = happyGoto action_148
action_98 (20) = happyGoto action_78
action_98 (21) = happyGoto action_79
action_98 (23) = happyGoto action_80
action_98 (24) = happyGoto action_81
action_98 (26) = happyGoto action_82
action_98 (28) = happyGoto action_149
action_98 (29) = happyGoto action_84
action_98 (30) = happyGoto action_85
action_98 (31) = happyGoto action_86
action_98 _ = happyFail

action_99 (49) = happyShift action_38
action_99 (50) = happyShift action_87
action_99 (53) = happyShift action_88
action_99 (59) = happyShift action_89
action_99 (61) = happyShift action_90
action_99 (69) = happyShift action_91
action_99 (70) = happyShift action_92
action_99 (71) = happyShift action_93
action_99 (72) = happyShift action_94
action_99 (73) = happyShift action_95
action_99 (74) = happyShift action_96
action_99 (75) = happyShift action_97
action_99 (79) = happyShift action_98
action_99 (81) = happyShift action_99
action_99 (83) = happyShift action_100
action_99 (87) = happyShift action_52
action_99 (91) = happyShift action_53
action_99 (92) = happyShift action_54
action_99 (93) = happyShift action_55
action_99 (95) = happyShift action_56
action_99 (96) = happyShift action_57
action_99 (98) = happyShift action_101
action_99 (99) = happyShift action_102
action_99 (17) = happyGoto action_144
action_99 (19) = happyGoto action_145
action_99 (20) = happyGoto action_78
action_99 (21) = happyGoto action_79
action_99 (23) = happyGoto action_80
action_99 (24) = happyGoto action_81
action_99 (26) = happyGoto action_82
action_99 (27) = happyGoto action_146
action_99 (28) = happyGoto action_104
action_99 (29) = happyGoto action_84
action_99 (30) = happyGoto action_85
action_99 (31) = happyGoto action_86
action_99 _ = happyFail

action_100 (49) = happyShift action_38
action_100 (50) = happyShift action_87
action_100 (53) = happyShift action_88
action_100 (59) = happyShift action_89
action_100 (61) = happyShift action_90
action_100 (69) = happyShift action_91
action_100 (70) = happyShift action_92
action_100 (71) = happyShift action_93
action_100 (72) = happyShift action_94
action_100 (73) = happyShift action_95
action_100 (74) = happyShift action_96
action_100 (75) = happyShift action_97
action_100 (79) = happyShift action_98
action_100 (81) = happyShift action_99
action_100 (83) = happyShift action_100
action_100 (87) = happyShift action_52
action_100 (91) = happyShift action_53
action_100 (92) = happyShift action_54
action_100 (93) = happyShift action_55
action_100 (95) = happyShift action_56
action_100 (96) = happyShift action_57
action_100 (98) = happyShift action_101
action_100 (99) = happyShift action_102
action_100 (17) = happyGoto action_77
action_100 (20) = happyGoto action_78
action_100 (21) = happyGoto action_79
action_100 (22) = happyGoto action_142
action_100 (23) = happyGoto action_80
action_100 (24) = happyGoto action_81
action_100 (26) = happyGoto action_82
action_100 (28) = happyGoto action_143
action_100 (29) = happyGoto action_84
action_100 (30) = happyGoto action_85
action_100 (31) = happyGoto action_86
action_100 _ = happyFail

action_101 (49) = happyShift action_38
action_101 (17) = happyGoto action_140
action_101 (18) = happyGoto action_141
action_101 _ = happyFail

action_102 _ = happyReduce_90

action_103 _ = happyReduce_29

action_104 (84) = happyShift action_139
action_104 _ = happyReduce_66

action_105 (39) = happyGoto action_137
action_105 (40) = happyGoto action_138
action_105 _ = happyReduce_112

action_106 _ = happyReduce_131

action_107 (78) = happyShift action_136
action_107 _ = happyFail

action_108 (85) = happyShift action_135
action_108 _ = happyReduce_128

action_109 (78) = happyShift action_134
action_109 _ = happyFail

action_110 _ = happyReduce_28

action_111 (86) = happyShift action_133
action_111 _ = happyFail

action_112 (86) = happyShift action_132
action_112 _ = happyFail

action_113 (49) = happyShift action_38
action_113 (50) = happyShift action_87
action_113 (53) = happyShift action_88
action_113 (59) = happyShift action_89
action_113 (61) = happyShift action_90
action_113 (69) = happyShift action_91
action_113 (70) = happyShift action_92
action_113 (71) = happyShift action_93
action_113 (72) = happyShift action_94
action_113 (73) = happyShift action_95
action_113 (74) = happyShift action_96
action_113 (75) = happyShift action_97
action_113 (79) = happyShift action_98
action_113 (81) = happyShift action_99
action_113 (83) = happyShift action_100
action_113 (87) = happyShift action_52
action_113 (91) = happyShift action_53
action_113 (92) = happyShift action_54
action_113 (93) = happyShift action_55
action_113 (95) = happyShift action_56
action_113 (96) = happyShift action_57
action_113 (98) = happyShift action_101
action_113 (99) = happyShift action_102
action_113 (17) = happyGoto action_77
action_113 (20) = happyGoto action_78
action_113 (21) = happyGoto action_79
action_113 (23) = happyGoto action_80
action_113 (24) = happyGoto action_81
action_113 (26) = happyGoto action_82
action_113 (28) = happyGoto action_131
action_113 (29) = happyGoto action_84
action_113 (30) = happyGoto action_85
action_113 (31) = happyGoto action_86
action_113 _ = happyFail

action_114 (49) = happyShift action_38
action_114 (50) = happyShift action_87
action_114 (53) = happyShift action_88
action_114 (59) = happyShift action_89
action_114 (61) = happyShift action_90
action_114 (69) = happyShift action_91
action_114 (70) = happyShift action_92
action_114 (71) = happyShift action_93
action_114 (72) = happyShift action_94
action_114 (73) = happyShift action_95
action_114 (74) = happyShift action_96
action_114 (75) = happyShift action_97
action_114 (79) = happyShift action_98
action_114 (81) = happyShift action_99
action_114 (83) = happyShift action_100
action_114 (87) = happyShift action_52
action_114 (91) = happyShift action_53
action_114 (92) = happyShift action_54
action_114 (93) = happyShift action_55
action_114 (95) = happyShift action_56
action_114 (96) = happyShift action_57
action_114 (98) = happyShift action_101
action_114 (99) = happyShift action_102
action_114 (17) = happyGoto action_77
action_114 (20) = happyGoto action_78
action_114 (21) = happyGoto action_79
action_114 (23) = happyGoto action_80
action_114 (24) = happyGoto action_81
action_114 (26) = happyGoto action_82
action_114 (28) = happyGoto action_130
action_114 (29) = happyGoto action_84
action_114 (30) = happyGoto action_85
action_114 (31) = happyGoto action_86
action_114 _ = happyFail

action_115 (84) = happyShift action_129
action_115 _ = happyReduce_36

action_116 (86) = happyShift action_128
action_116 _ = happyFail

action_117 (49) = happyShift action_38
action_117 (17) = happyGoto action_127
action_117 _ = happyFail

action_118 (49) = happyShift action_38
action_118 (17) = happyGoto action_115
action_118 (19) = happyGoto action_126
action_118 _ = happyFail

action_119 (86) = happyShift action_125
action_119 _ = happyFail

action_120 (49) = happyShift action_38
action_120 (50) = happyShift action_87
action_120 (53) = happyShift action_88
action_120 (59) = happyShift action_89
action_120 (61) = happyShift action_90
action_120 (69) = happyShift action_91
action_120 (70) = happyShift action_92
action_120 (71) = happyShift action_93
action_120 (72) = happyShift action_94
action_120 (73) = happyShift action_95
action_120 (74) = happyShift action_96
action_120 (75) = happyShift action_97
action_120 (79) = happyShift action_98
action_120 (81) = happyShift action_99
action_120 (83) = happyShift action_100
action_120 (87) = happyShift action_52
action_120 (91) = happyShift action_53
action_120 (92) = happyShift action_54
action_120 (93) = happyShift action_55
action_120 (95) = happyShift action_56
action_120 (96) = happyShift action_57
action_120 (98) = happyShift action_101
action_120 (99) = happyShift action_102
action_120 (17) = happyGoto action_77
action_120 (20) = happyGoto action_78
action_120 (21) = happyGoto action_79
action_120 (23) = happyGoto action_80
action_120 (24) = happyGoto action_81
action_120 (26) = happyGoto action_82
action_120 (28) = happyGoto action_124
action_120 (29) = happyGoto action_84
action_120 (30) = happyGoto action_85
action_120 (31) = happyGoto action_86
action_120 _ = happyFail

action_121 _ = happyReduce_139

action_122 (49) = happyShift action_38
action_122 (17) = happyGoto action_115
action_122 (19) = happyGoto action_123
action_122 _ = happyFail

action_123 (86) = happyShift action_224
action_123 _ = happyFail

action_124 (77) = happyShift action_223
action_124 _ = happyFail

action_125 (49) = happyShift action_38
action_125 (50) = happyShift action_87
action_125 (53) = happyShift action_88
action_125 (59) = happyShift action_89
action_125 (61) = happyShift action_90
action_125 (69) = happyShift action_91
action_125 (70) = happyShift action_92
action_125 (71) = happyShift action_93
action_125 (72) = happyShift action_94
action_125 (73) = happyShift action_95
action_125 (74) = happyShift action_96
action_125 (75) = happyShift action_97
action_125 (79) = happyShift action_98
action_125 (81) = happyShift action_99
action_125 (83) = happyShift action_100
action_125 (87) = happyShift action_52
action_125 (91) = happyShift action_53
action_125 (92) = happyShift action_54
action_125 (93) = happyShift action_55
action_125 (95) = happyShift action_56
action_125 (96) = happyShift action_57
action_125 (98) = happyShift action_101
action_125 (99) = happyShift action_102
action_125 (17) = happyGoto action_77
action_125 (20) = happyGoto action_78
action_125 (21) = happyGoto action_79
action_125 (23) = happyGoto action_80
action_125 (24) = happyGoto action_81
action_125 (26) = happyGoto action_82
action_125 (28) = happyGoto action_222
action_125 (29) = happyGoto action_84
action_125 (30) = happyGoto action_85
action_125 (31) = happyGoto action_86
action_125 _ = happyFail

action_126 (86) = happyShift action_221
action_126 _ = happyFail

action_127 (82) = happyShift action_220
action_127 _ = happyFail

action_128 (49) = happyShift action_38
action_128 (50) = happyShift action_87
action_128 (53) = happyShift action_88
action_128 (59) = happyShift action_89
action_128 (61) = happyShift action_90
action_128 (69) = happyShift action_91
action_128 (70) = happyShift action_92
action_128 (71) = happyShift action_93
action_128 (72) = happyShift action_94
action_128 (73) = happyShift action_95
action_128 (74) = happyShift action_96
action_128 (75) = happyShift action_97
action_128 (79) = happyShift action_98
action_128 (81) = happyShift action_99
action_128 (83) = happyShift action_100
action_128 (87) = happyShift action_52
action_128 (91) = happyShift action_53
action_128 (92) = happyShift action_54
action_128 (93) = happyShift action_55
action_128 (95) = happyShift action_56
action_128 (96) = happyShift action_57
action_128 (98) = happyShift action_101
action_128 (99) = happyShift action_102
action_128 (17) = happyGoto action_77
action_128 (20) = happyGoto action_78
action_128 (21) = happyGoto action_79
action_128 (23) = happyGoto action_80
action_128 (24) = happyGoto action_81
action_128 (26) = happyGoto action_82
action_128 (28) = happyGoto action_219
action_128 (29) = happyGoto action_84
action_128 (30) = happyGoto action_85
action_128 (31) = happyGoto action_86
action_128 _ = happyFail

action_129 (49) = happyShift action_38
action_129 (17) = happyGoto action_115
action_129 (19) = happyGoto action_218
action_129 _ = happyFail

action_130 (77) = happyShift action_217
action_130 _ = happyFail

action_131 (77) = happyShift action_216
action_131 _ = happyFail

action_132 (49) = happyShift action_38
action_132 (50) = happyShift action_87
action_132 (53) = happyShift action_88
action_132 (59) = happyShift action_89
action_132 (61) = happyShift action_90
action_132 (69) = happyShift action_91
action_132 (70) = happyShift action_92
action_132 (71) = happyShift action_93
action_132 (72) = happyShift action_94
action_132 (73) = happyShift action_95
action_132 (74) = happyShift action_96
action_132 (75) = happyShift action_97
action_132 (79) = happyShift action_98
action_132 (81) = happyShift action_99
action_132 (83) = happyShift action_100
action_132 (87) = happyShift action_52
action_132 (91) = happyShift action_53
action_132 (92) = happyShift action_54
action_132 (93) = happyShift action_55
action_132 (95) = happyShift action_56
action_132 (96) = happyShift action_57
action_132 (98) = happyShift action_101
action_132 (99) = happyShift action_102
action_132 (17) = happyGoto action_77
action_132 (20) = happyGoto action_78
action_132 (21) = happyGoto action_79
action_132 (23) = happyGoto action_80
action_132 (24) = happyGoto action_81
action_132 (26) = happyGoto action_82
action_132 (28) = happyGoto action_215
action_132 (29) = happyGoto action_84
action_132 (30) = happyGoto action_85
action_132 (31) = happyGoto action_86
action_132 _ = happyFail

action_133 (49) = happyShift action_38
action_133 (50) = happyShift action_87
action_133 (53) = happyShift action_88
action_133 (59) = happyShift action_89
action_133 (61) = happyShift action_90
action_133 (69) = happyShift action_91
action_133 (70) = happyShift action_92
action_133 (71) = happyShift action_93
action_133 (72) = happyShift action_94
action_133 (73) = happyShift action_95
action_133 (74) = happyShift action_96
action_133 (75) = happyShift action_97
action_133 (79) = happyShift action_98
action_133 (81) = happyShift action_99
action_133 (83) = happyShift action_100
action_133 (87) = happyShift action_52
action_133 (91) = happyShift action_53
action_133 (92) = happyShift action_54
action_133 (93) = happyShift action_55
action_133 (95) = happyShift action_56
action_133 (96) = happyShift action_57
action_133 (98) = happyShift action_101
action_133 (99) = happyShift action_102
action_133 (17) = happyGoto action_77
action_133 (20) = happyGoto action_78
action_133 (21) = happyGoto action_79
action_133 (23) = happyGoto action_80
action_133 (24) = happyGoto action_81
action_133 (26) = happyGoto action_82
action_133 (28) = happyGoto action_214
action_133 (29) = happyGoto action_84
action_133 (30) = happyGoto action_85
action_133 (31) = happyGoto action_86
action_133 _ = happyFail

action_134 _ = happyReduce_26

action_135 (49) = happyShift action_38
action_135 (17) = happyGoto action_105
action_135 (38) = happyGoto action_213
action_135 _ = happyReduce_130

action_136 _ = happyReduce_27

action_137 (90) = happyShift action_212
action_137 _ = happyReduce_110

action_138 (49) = happyShift action_38
action_138 (73) = happyShift action_209
action_138 (81) = happyShift action_210
action_138 (87) = happyShift action_211
action_138 (17) = happyGoto action_207
action_138 (41) = happyGoto action_208
action_138 _ = happyReduce_111

action_139 (49) = happyShift action_38
action_139 (50) = happyShift action_87
action_139 (53) = happyShift action_88
action_139 (59) = happyShift action_89
action_139 (61) = happyShift action_90
action_139 (69) = happyShift action_91
action_139 (70) = happyShift action_92
action_139 (71) = happyShift action_93
action_139 (72) = happyShift action_94
action_139 (73) = happyShift action_95
action_139 (74) = happyShift action_96
action_139 (75) = happyShift action_97
action_139 (79) = happyShift action_98
action_139 (81) = happyShift action_99
action_139 (83) = happyShift action_100
action_139 (87) = happyShift action_52
action_139 (91) = happyShift action_53
action_139 (92) = happyShift action_54
action_139 (93) = happyShift action_55
action_139 (95) = happyShift action_56
action_139 (96) = happyShift action_57
action_139 (98) = happyShift action_101
action_139 (99) = happyShift action_102
action_139 (17) = happyGoto action_77
action_139 (20) = happyGoto action_78
action_139 (21) = happyGoto action_79
action_139 (23) = happyGoto action_80
action_139 (24) = happyGoto action_81
action_139 (26) = happyGoto action_82
action_139 (27) = happyGoto action_206
action_139 (28) = happyGoto action_104
action_139 (29) = happyGoto action_84
action_139 (30) = happyGoto action_85
action_139 (31) = happyGoto action_86
action_139 _ = happyFail

action_140 (49) = happyShift action_38
action_140 (17) = happyGoto action_140
action_140 (18) = happyGoto action_205
action_140 _ = happyReduce_34

action_141 (88) = happyShift action_204
action_141 _ = happyFail

action_142 _ = happyReduce_44

action_143 (83) = happyShift action_202
action_143 (84) = happyShift action_203
action_143 _ = happyFail

action_144 (75) = happyShift action_200
action_144 (84) = happyShift action_129
action_144 (86) = happyReduce_36
action_144 (89) = happyShift action_201
action_144 _ = happyReduce_87

action_145 (86) = happyShift action_199
action_145 _ = happyFail

action_146 (82) = happyShift action_198
action_146 _ = happyFail

action_147 (75) = happyShift action_196
action_147 (84) = happyShift action_129
action_147 (86) = happyReduce_36
action_147 (89) = happyShift action_197
action_147 _ = happyReduce_87

action_148 (86) = happyShift action_195
action_148 _ = happyFail

action_149 (80) = happyShift action_194
action_149 _ = happyFail

action_150 (86) = happyShift action_193
action_150 _ = happyFail

action_151 _ = happyReduce_91

action_152 _ = happyReduce_92

action_153 (49) = happyShift action_38
action_153 (50) = happyShift action_87
action_153 (53) = happyShift action_88
action_153 (59) = happyShift action_89
action_153 (61) = happyShift action_90
action_153 (69) = happyShift action_91
action_153 (70) = happyShift action_92
action_153 (71) = happyShift action_93
action_153 (72) = happyShift action_94
action_153 (73) = happyShift action_95
action_153 (74) = happyShift action_96
action_153 (75) = happyShift action_97
action_153 (79) = happyShift action_98
action_153 (81) = happyShift action_99
action_153 (83) = happyShift action_100
action_153 (87) = happyShift action_52
action_153 (91) = happyShift action_53
action_153 (92) = happyShift action_54
action_153 (93) = happyShift action_55
action_153 (95) = happyShift action_56
action_153 (96) = happyShift action_57
action_153 (98) = happyShift action_101
action_153 (99) = happyShift action_102
action_153 (17) = happyGoto action_77
action_153 (20) = happyGoto action_78
action_153 (21) = happyGoto action_79
action_153 (23) = happyGoto action_80
action_153 (24) = happyGoto action_81
action_153 (26) = happyGoto action_82
action_153 (27) = happyGoto action_146
action_153 (28) = happyGoto action_104
action_153 (29) = happyGoto action_84
action_153 (30) = happyGoto action_85
action_153 (31) = happyGoto action_86
action_153 _ = happyFail

action_154 _ = happyReduce_75

action_155 _ = happyReduce_77

action_156 (86) = happyShift action_192
action_156 _ = happyFail

action_157 (81) = happyShift action_191
action_157 _ = happyFail

action_158 (90) = happyShift action_190
action_158 _ = happyFail

action_159 (49) = happyShift action_38
action_159 (17) = happyGoto action_189
action_159 _ = happyFail

action_160 (49) = happyShift action_38
action_160 (17) = happyGoto action_188
action_160 _ = happyFail

action_161 (77) = happyShift action_187
action_161 _ = happyFail

action_162 (49) = happyShift action_38
action_162 (17) = happyGoto action_140
action_162 (18) = happyGoto action_184
action_162 (32) = happyGoto action_185
action_162 (33) = happyGoto action_186
action_162 _ = happyReduce_96

action_163 (49) = happyShift action_38
action_163 (50) = happyShift action_87
action_163 (53) = happyShift action_88
action_163 (69) = happyShift action_91
action_163 (70) = happyShift action_92
action_163 (71) = happyShift action_93
action_163 (72) = happyShift action_94
action_163 (73) = happyShift action_95
action_163 (74) = happyShift action_96
action_163 (75) = happyShift action_97
action_163 (79) = happyShift action_98
action_163 (81) = happyShift action_99
action_163 (83) = happyShift action_100
action_163 (87) = happyShift action_52
action_163 (91) = happyShift action_53
action_163 (92) = happyShift action_54
action_163 (93) = happyShift action_55
action_163 (95) = happyShift action_56
action_163 (96) = happyShift action_57
action_163 (99) = happyShift action_102
action_163 (17) = happyGoto action_77
action_163 (20) = happyGoto action_78
action_163 (21) = happyGoto action_79
action_163 (23) = happyGoto action_80
action_163 (24) = happyGoto action_81
action_163 (26) = happyGoto action_172
action_163 (29) = happyGoto action_183
action_163 (30) = happyGoto action_85
action_163 (31) = happyGoto action_86
action_163 _ = happyFail

action_164 _ = happyReduce_81

action_165 _ = happyReduce_83

action_166 (49) = happyShift action_38
action_166 (17) = happyGoto action_182
action_166 _ = happyFail

action_167 (49) = happyShift action_38
action_167 (50) = happyShift action_87
action_167 (53) = happyShift action_88
action_167 (59) = happyShift action_89
action_167 (61) = happyShift action_90
action_167 (69) = happyShift action_91
action_167 (70) = happyShift action_92
action_167 (71) = happyShift action_93
action_167 (72) = happyShift action_94
action_167 (73) = happyShift action_95
action_167 (74) = happyShift action_96
action_167 (75) = happyShift action_97
action_167 (79) = happyShift action_98
action_167 (81) = happyShift action_99
action_167 (83) = happyShift action_100
action_167 (87) = happyShift action_52
action_167 (91) = happyShift action_53
action_167 (92) = happyShift action_54
action_167 (93) = happyShift action_55
action_167 (95) = happyShift action_56
action_167 (96) = happyShift action_57
action_167 (98) = happyShift action_101
action_167 (99) = happyShift action_102
action_167 (17) = happyGoto action_77
action_167 (20) = happyGoto action_78
action_167 (21) = happyGoto action_79
action_167 (23) = happyGoto action_80
action_167 (24) = happyGoto action_81
action_167 (26) = happyGoto action_82
action_167 (28) = happyGoto action_181
action_167 (29) = happyGoto action_84
action_167 (30) = happyGoto action_85
action_167 (31) = happyGoto action_86
action_167 _ = happyFail

action_168 (49) = happyShift action_38
action_168 (50) = happyShift action_87
action_168 (53) = happyShift action_88
action_168 (59) = happyShift action_89
action_168 (61) = happyShift action_90
action_168 (69) = happyShift action_91
action_168 (70) = happyShift action_92
action_168 (71) = happyShift action_93
action_168 (72) = happyShift action_94
action_168 (73) = happyShift action_95
action_168 (74) = happyShift action_96
action_168 (75) = happyShift action_97
action_168 (79) = happyShift action_98
action_168 (81) = happyShift action_99
action_168 (83) = happyShift action_100
action_168 (87) = happyShift action_52
action_168 (91) = happyShift action_53
action_168 (92) = happyShift action_54
action_168 (93) = happyShift action_55
action_168 (95) = happyShift action_56
action_168 (96) = happyShift action_57
action_168 (98) = happyShift action_101
action_168 (99) = happyShift action_102
action_168 (17) = happyGoto action_77
action_168 (20) = happyGoto action_78
action_168 (21) = happyGoto action_79
action_168 (23) = happyGoto action_80
action_168 (24) = happyGoto action_81
action_168 (26) = happyGoto action_82
action_168 (28) = happyGoto action_180
action_168 (29) = happyGoto action_84
action_168 (30) = happyGoto action_85
action_168 (31) = happyGoto action_86
action_168 _ = happyFail

action_169 (49) = happyShift action_38
action_169 (50) = happyShift action_87
action_169 (53) = happyShift action_88
action_169 (69) = happyShift action_91
action_169 (70) = happyShift action_92
action_169 (71) = happyShift action_93
action_169 (72) = happyShift action_94
action_169 (73) = happyShift action_95
action_169 (74) = happyShift action_96
action_169 (75) = happyShift action_97
action_169 (79) = happyShift action_98
action_169 (81) = happyShift action_99
action_169 (83) = happyShift action_100
action_169 (87) = happyShift action_52
action_169 (91) = happyShift action_53
action_169 (92) = happyShift action_54
action_169 (93) = happyShift action_55
action_169 (95) = happyShift action_56
action_169 (96) = happyShift action_57
action_169 (99) = happyShift action_102
action_169 (17) = happyGoto action_77
action_169 (20) = happyGoto action_78
action_169 (21) = happyGoto action_79
action_169 (23) = happyGoto action_80
action_169 (24) = happyGoto action_81
action_169 (26) = happyGoto action_172
action_169 (29) = happyGoto action_179
action_169 (30) = happyGoto action_85
action_169 (31) = happyGoto action_86
action_169 _ = happyFail

action_170 (83) = happyShift action_100
action_170 (21) = happyGoto action_178
action_170 _ = happyFail

action_171 (83) = happyShift action_100
action_171 (21) = happyGoto action_177
action_171 _ = happyFail

action_172 (97) = happyShift action_169
action_172 _ = happyFail

action_173 (97) = happyReduce_62
action_173 _ = happyReduce_62

action_174 (49) = happyShift action_38
action_174 (50) = happyShift action_87
action_174 (53) = happyShift action_88
action_174 (59) = happyShift action_89
action_174 (61) = happyShift action_90
action_174 (69) = happyShift action_91
action_174 (70) = happyShift action_92
action_174 (71) = happyShift action_93
action_174 (72) = happyShift action_94
action_174 (73) = happyShift action_95
action_174 (74) = happyShift action_96
action_174 (75) = happyShift action_97
action_174 (79) = happyShift action_98
action_174 (81) = happyShift action_99
action_174 (83) = happyShift action_100
action_174 (87) = happyShift action_52
action_174 (91) = happyShift action_53
action_174 (92) = happyShift action_54
action_174 (93) = happyShift action_55
action_174 (95) = happyShift action_56
action_174 (96) = happyShift action_57
action_174 (98) = happyShift action_101
action_174 (99) = happyShift action_102
action_174 (17) = happyGoto action_144
action_174 (19) = happyGoto action_176
action_174 (20) = happyGoto action_78
action_174 (21) = happyGoto action_79
action_174 (23) = happyGoto action_80
action_174 (24) = happyGoto action_81
action_174 (26) = happyGoto action_82
action_174 (27) = happyGoto action_146
action_174 (28) = happyGoto action_104
action_174 (29) = happyGoto action_84
action_174 (30) = happyGoto action_85
action_174 (31) = happyGoto action_86
action_174 _ = happyFail

action_175 _ = happyReduce_30

action_176 (86) = happyShift action_265
action_176 _ = happyFail

action_177 _ = happyReduce_48

action_178 _ = happyReduce_47

action_179 (97) = happyReduce_79
action_179 _ = happyReduce_79

action_180 _ = happyReduce_68

action_181 _ = happyReduce_73

action_182 _ = happyReduce_82

action_183 (97) = happyReduce_78
action_183 _ = happyReduce_78

action_184 (90) = happyShift action_264
action_184 _ = happyFail

action_185 (78) = happyShift action_263
action_185 _ = happyFail

action_186 (85) = happyShift action_262
action_186 _ = happyReduce_95

action_187 (49) = happyShift action_38
action_187 (73) = happyShift action_209
action_187 (81) = happyShift action_210
action_187 (87) = happyShift action_211
action_187 (17) = happyGoto action_207
action_187 (37) = happyGoto action_260
action_187 (41) = happyGoto action_261
action_187 _ = happyReduce_108

action_188 (86) = happyShift action_259
action_188 _ = happyFail

action_189 (86) = happyShift action_258
action_189 _ = happyFail

action_190 (49) = happyShift action_38
action_190 (50) = happyShift action_87
action_190 (53) = happyShift action_88
action_190 (59) = happyShift action_89
action_190 (61) = happyShift action_90
action_190 (69) = happyShift action_91
action_190 (70) = happyShift action_92
action_190 (71) = happyShift action_93
action_190 (72) = happyShift action_94
action_190 (73) = happyShift action_95
action_190 (74) = happyShift action_96
action_190 (75) = happyShift action_97
action_190 (79) = happyShift action_98
action_190 (81) = happyShift action_99
action_190 (83) = happyShift action_100
action_190 (87) = happyShift action_52
action_190 (91) = happyShift action_53
action_190 (92) = happyShift action_54
action_190 (93) = happyShift action_55
action_190 (95) = happyShift action_56
action_190 (96) = happyShift action_57
action_190 (98) = happyShift action_101
action_190 (99) = happyShift action_102
action_190 (17) = happyGoto action_77
action_190 (20) = happyGoto action_78
action_190 (21) = happyGoto action_79
action_190 (23) = happyGoto action_80
action_190 (24) = happyGoto action_81
action_190 (26) = happyGoto action_82
action_190 (27) = happyGoto action_257
action_190 (28) = happyGoto action_104
action_190 (29) = happyGoto action_84
action_190 (30) = happyGoto action_85
action_190 (31) = happyGoto action_86
action_190 _ = happyFail

action_191 (49) = happyShift action_38
action_191 (17) = happyGoto action_256
action_191 _ = happyFail

action_192 (49) = happyShift action_38
action_192 (50) = happyShift action_87
action_192 (53) = happyShift action_88
action_192 (59) = happyShift action_89
action_192 (61) = happyShift action_90
action_192 (69) = happyShift action_91
action_192 (70) = happyShift action_92
action_192 (71) = happyShift action_93
action_192 (72) = happyShift action_94
action_192 (73) = happyShift action_95
action_192 (74) = happyShift action_96
action_192 (75) = happyShift action_97
action_192 (79) = happyShift action_98
action_192 (81) = happyShift action_99
action_192 (83) = happyShift action_100
action_192 (87) = happyShift action_52
action_192 (91) = happyShift action_53
action_192 (92) = happyShift action_54
action_192 (93) = happyShift action_55
action_192 (95) = happyShift action_56
action_192 (96) = happyShift action_57
action_192 (98) = happyShift action_101
action_192 (99) = happyShift action_102
action_192 (17) = happyGoto action_77
action_192 (20) = happyGoto action_78
action_192 (21) = happyGoto action_79
action_192 (23) = happyGoto action_80
action_192 (24) = happyGoto action_81
action_192 (26) = happyGoto action_82
action_192 (28) = happyGoto action_255
action_192 (29) = happyGoto action_84
action_192 (30) = happyGoto action_85
action_192 (31) = happyGoto action_86
action_192 _ = happyFail

action_193 (49) = happyShift action_38
action_193 (50) = happyShift action_87
action_193 (53) = happyShift action_88
action_193 (59) = happyShift action_89
action_193 (61) = happyShift action_90
action_193 (69) = happyShift action_91
action_193 (70) = happyShift action_92
action_193 (71) = happyShift action_93
action_193 (72) = happyShift action_94
action_193 (73) = happyShift action_95
action_193 (74) = happyShift action_96
action_193 (75) = happyShift action_97
action_193 (79) = happyShift action_98
action_193 (81) = happyShift action_99
action_193 (83) = happyShift action_100
action_193 (87) = happyShift action_52
action_193 (91) = happyShift action_53
action_193 (92) = happyShift action_54
action_193 (93) = happyShift action_55
action_193 (95) = happyShift action_56
action_193 (96) = happyShift action_57
action_193 (98) = happyShift action_101
action_193 (99) = happyShift action_102
action_193 (17) = happyGoto action_77
action_193 (20) = happyGoto action_78
action_193 (21) = happyGoto action_79
action_193 (23) = happyGoto action_80
action_193 (24) = happyGoto action_81
action_193 (26) = happyGoto action_82
action_193 (28) = happyGoto action_254
action_193 (29) = happyGoto action_84
action_193 (30) = happyGoto action_85
action_193 (31) = happyGoto action_86
action_193 _ = happyFail

action_194 _ = happyReduce_61

action_195 (49) = happyShift action_38
action_195 (50) = happyShift action_87
action_195 (53) = happyShift action_88
action_195 (59) = happyShift action_89
action_195 (61) = happyShift action_90
action_195 (69) = happyShift action_91
action_195 (70) = happyShift action_92
action_195 (71) = happyShift action_93
action_195 (72) = happyShift action_94
action_195 (73) = happyShift action_95
action_195 (74) = happyShift action_96
action_195 (75) = happyShift action_97
action_195 (79) = happyShift action_98
action_195 (81) = happyShift action_99
action_195 (83) = happyShift action_100
action_195 (87) = happyShift action_52
action_195 (91) = happyShift action_53
action_195 (92) = happyShift action_54
action_195 (93) = happyShift action_55
action_195 (95) = happyShift action_56
action_195 (96) = happyShift action_57
action_195 (98) = happyShift action_101
action_195 (99) = happyShift action_102
action_195 (17) = happyGoto action_77
action_195 (20) = happyGoto action_78
action_195 (21) = happyGoto action_79
action_195 (23) = happyGoto action_80
action_195 (24) = happyGoto action_81
action_195 (26) = happyGoto action_82
action_195 (28) = happyGoto action_253
action_195 (29) = happyGoto action_84
action_195 (30) = happyGoto action_85
action_195 (31) = happyGoto action_86
action_195 _ = happyFail

action_196 (49) = happyShift action_38
action_196 (50) = happyShift action_87
action_196 (53) = happyShift action_88
action_196 (59) = happyShift action_89
action_196 (61) = happyShift action_90
action_196 (69) = happyShift action_91
action_196 (70) = happyShift action_92
action_196 (71) = happyShift action_93
action_196 (72) = happyShift action_94
action_196 (73) = happyShift action_95
action_196 (74) = happyShift action_96
action_196 (75) = happyShift action_97
action_196 (79) = happyShift action_98
action_196 (81) = happyShift action_99
action_196 (83) = happyShift action_100
action_196 (87) = happyShift action_52
action_196 (91) = happyShift action_53
action_196 (92) = happyShift action_54
action_196 (93) = happyShift action_55
action_196 (95) = happyShift action_56
action_196 (96) = happyShift action_57
action_196 (98) = happyShift action_101
action_196 (99) = happyShift action_102
action_196 (17) = happyGoto action_77
action_196 (20) = happyGoto action_78
action_196 (21) = happyGoto action_79
action_196 (23) = happyGoto action_80
action_196 (24) = happyGoto action_81
action_196 (26) = happyGoto action_82
action_196 (28) = happyGoto action_252
action_196 (29) = happyGoto action_84
action_196 (30) = happyGoto action_85
action_196 (31) = happyGoto action_86
action_196 _ = happyFail

action_197 (49) = happyShift action_38
action_197 (50) = happyShift action_87
action_197 (53) = happyShift action_88
action_197 (59) = happyShift action_89
action_197 (61) = happyShift action_90
action_197 (69) = happyShift action_91
action_197 (70) = happyShift action_92
action_197 (71) = happyShift action_93
action_197 (72) = happyShift action_94
action_197 (73) = happyShift action_95
action_197 (74) = happyShift action_96
action_197 (75) = happyShift action_97
action_197 (79) = happyShift action_98
action_197 (81) = happyShift action_99
action_197 (83) = happyShift action_100
action_197 (87) = happyShift action_52
action_197 (91) = happyShift action_53
action_197 (92) = happyShift action_54
action_197 (93) = happyShift action_55
action_197 (95) = happyShift action_56
action_197 (96) = happyShift action_57
action_197 (98) = happyShift action_101
action_197 (99) = happyShift action_102
action_197 (17) = happyGoto action_77
action_197 (20) = happyGoto action_78
action_197 (21) = happyGoto action_79
action_197 (23) = happyGoto action_80
action_197 (24) = happyGoto action_81
action_197 (26) = happyGoto action_82
action_197 (28) = happyGoto action_251
action_197 (29) = happyGoto action_84
action_197 (30) = happyGoto action_85
action_197 (31) = happyGoto action_86
action_197 _ = happyFail

action_198 _ = happyReduce_89

action_199 (49) = happyShift action_38
action_199 (50) = happyShift action_87
action_199 (53) = happyShift action_88
action_199 (59) = happyShift action_89
action_199 (61) = happyShift action_90
action_199 (69) = happyShift action_91
action_199 (70) = happyShift action_92
action_199 (71) = happyShift action_93
action_199 (72) = happyShift action_94
action_199 (73) = happyShift action_95
action_199 (74) = happyShift action_96
action_199 (75) = happyShift action_97
action_199 (79) = happyShift action_98
action_199 (81) = happyShift action_99
action_199 (83) = happyShift action_100
action_199 (87) = happyShift action_52
action_199 (91) = happyShift action_53
action_199 (92) = happyShift action_54
action_199 (93) = happyShift action_55
action_199 (95) = happyShift action_56
action_199 (96) = happyShift action_57
action_199 (98) = happyShift action_101
action_199 (99) = happyShift action_102
action_199 (17) = happyGoto action_77
action_199 (20) = happyGoto action_78
action_199 (21) = happyGoto action_79
action_199 (23) = happyGoto action_80
action_199 (24) = happyGoto action_81
action_199 (26) = happyGoto action_82
action_199 (28) = happyGoto action_250
action_199 (29) = happyGoto action_84
action_199 (30) = happyGoto action_85
action_199 (31) = happyGoto action_86
action_199 _ = happyFail

action_200 (49) = happyShift action_38
action_200 (50) = happyShift action_87
action_200 (53) = happyShift action_88
action_200 (59) = happyShift action_89
action_200 (61) = happyShift action_90
action_200 (69) = happyShift action_91
action_200 (70) = happyShift action_92
action_200 (71) = happyShift action_93
action_200 (72) = happyShift action_94
action_200 (73) = happyShift action_95
action_200 (74) = happyShift action_96
action_200 (75) = happyShift action_97
action_200 (79) = happyShift action_98
action_200 (81) = happyShift action_99
action_200 (83) = happyShift action_100
action_200 (87) = happyShift action_52
action_200 (91) = happyShift action_53
action_200 (92) = happyShift action_54
action_200 (93) = happyShift action_55
action_200 (95) = happyShift action_56
action_200 (96) = happyShift action_57
action_200 (98) = happyShift action_101
action_200 (99) = happyShift action_102
action_200 (17) = happyGoto action_77
action_200 (20) = happyGoto action_78
action_200 (21) = happyGoto action_79
action_200 (23) = happyGoto action_80
action_200 (24) = happyGoto action_81
action_200 (26) = happyGoto action_82
action_200 (28) = happyGoto action_249
action_200 (29) = happyGoto action_84
action_200 (30) = happyGoto action_85
action_200 (31) = happyGoto action_86
action_200 _ = happyFail

action_201 (49) = happyShift action_38
action_201 (50) = happyShift action_87
action_201 (53) = happyShift action_88
action_201 (59) = happyShift action_89
action_201 (61) = happyShift action_90
action_201 (69) = happyShift action_91
action_201 (70) = happyShift action_92
action_201 (71) = happyShift action_93
action_201 (72) = happyShift action_94
action_201 (73) = happyShift action_95
action_201 (74) = happyShift action_96
action_201 (75) = happyShift action_97
action_201 (79) = happyShift action_98
action_201 (81) = happyShift action_99
action_201 (83) = happyShift action_100
action_201 (87) = happyShift action_52
action_201 (91) = happyShift action_53
action_201 (92) = happyShift action_54
action_201 (93) = happyShift action_55
action_201 (95) = happyShift action_56
action_201 (96) = happyShift action_57
action_201 (98) = happyShift action_101
action_201 (99) = happyShift action_102
action_201 (17) = happyGoto action_77
action_201 (20) = happyGoto action_78
action_201 (21) = happyGoto action_79
action_201 (23) = happyGoto action_80
action_201 (24) = happyGoto action_81
action_201 (26) = happyGoto action_82
action_201 (28) = happyGoto action_248
action_201 (29) = happyGoto action_84
action_201 (30) = happyGoto action_85
action_201 (31) = happyGoto action_86
action_201 _ = happyFail

action_202 _ = happyReduce_45

action_203 (49) = happyShift action_38
action_203 (50) = happyShift action_87
action_203 (53) = happyShift action_88
action_203 (59) = happyShift action_89
action_203 (61) = happyShift action_90
action_203 (69) = happyShift action_91
action_203 (70) = happyShift action_92
action_203 (71) = happyShift action_93
action_203 (72) = happyShift action_94
action_203 (73) = happyShift action_95
action_203 (74) = happyShift action_96
action_203 (75) = happyShift action_97
action_203 (79) = happyShift action_98
action_203 (81) = happyShift action_99
action_203 (83) = happyShift action_100
action_203 (87) = happyShift action_52
action_203 (91) = happyShift action_53
action_203 (92) = happyShift action_54
action_203 (93) = happyShift action_55
action_203 (95) = happyShift action_56
action_203 (96) = happyShift action_57
action_203 (98) = happyShift action_101
action_203 (99) = happyShift action_102
action_203 (17) = happyGoto action_77
action_203 (20) = happyGoto action_78
action_203 (21) = happyGoto action_79
action_203 (22) = happyGoto action_247
action_203 (23) = happyGoto action_80
action_203 (24) = happyGoto action_81
action_203 (26) = happyGoto action_82
action_203 (28) = happyGoto action_143
action_203 (29) = happyGoto action_84
action_203 (30) = happyGoto action_85
action_203 (31) = happyGoto action_86
action_203 _ = happyFail

action_204 (49) = happyShift action_38
action_204 (50) = happyShift action_87
action_204 (53) = happyShift action_88
action_204 (59) = happyShift action_89
action_204 (61) = happyShift action_90
action_204 (69) = happyShift action_91
action_204 (70) = happyShift action_92
action_204 (71) = happyShift action_93
action_204 (72) = happyShift action_94
action_204 (73) = happyShift action_95
action_204 (74) = happyShift action_96
action_204 (75) = happyShift action_97
action_204 (79) = happyShift action_98
action_204 (81) = happyShift action_99
action_204 (83) = happyShift action_100
action_204 (87) = happyShift action_52
action_204 (91) = happyShift action_53
action_204 (92) = happyShift action_54
action_204 (93) = happyShift action_55
action_204 (95) = happyShift action_56
action_204 (96) = happyShift action_57
action_204 (98) = happyShift action_101
action_204 (99) = happyShift action_102
action_204 (17) = happyGoto action_77
action_204 (20) = happyGoto action_78
action_204 (21) = happyGoto action_79
action_204 (23) = happyGoto action_80
action_204 (24) = happyGoto action_81
action_204 (26) = happyGoto action_82
action_204 (27) = happyGoto action_246
action_204 (28) = happyGoto action_104
action_204 (29) = happyGoto action_84
action_204 (30) = happyGoto action_85
action_204 (31) = happyGoto action_86
action_204 _ = happyFail

action_205 _ = happyReduce_35

action_206 _ = happyReduce_67

action_207 _ = happyReduce_116

action_208 _ = happyReduce_113

action_209 (49) = happyShift action_38
action_209 (73) = happyShift action_209
action_209 (81) = happyShift action_210
action_209 (87) = happyShift action_211
action_209 (17) = happyGoto action_207
action_209 (41) = happyGoto action_245
action_209 _ = happyFail

action_210 (49) = happyShift action_38
action_210 (73) = happyShift action_209
action_210 (81) = happyShift action_210
action_210 (82) = happyShift action_244
action_210 (87) = happyShift action_211
action_210 (17) = happyGoto action_239
action_210 (41) = happyGoto action_240
action_210 (42) = happyGoto action_241
action_210 (43) = happyGoto action_242
action_210 (44) = happyGoto action_243
action_210 _ = happyFail

action_211 (49) = happyShift action_38
action_211 (50) = happyShift action_152
action_211 (53) = happyShift action_88
action_211 (69) = happyShift action_238
action_211 (71) = happyShift action_93
action_211 (72) = happyShift action_94
action_211 (73) = happyShift action_95
action_211 (74) = happyShift action_96
action_211 (75) = happyShift action_97
action_211 (81) = happyShift action_153
action_211 (99) = happyShift action_102
action_211 (17) = happyGoto action_77
action_211 (31) = happyGoto action_237
action_211 _ = happyFail

action_212 (49) = happyShift action_38
action_212 (50) = happyShift action_87
action_212 (53) = happyShift action_88
action_212 (59) = happyShift action_89
action_212 (61) = happyShift action_90
action_212 (69) = happyShift action_91
action_212 (70) = happyShift action_92
action_212 (71) = happyShift action_93
action_212 (72) = happyShift action_94
action_212 (73) = happyShift action_95
action_212 (74) = happyShift action_96
action_212 (75) = happyShift action_97
action_212 (79) = happyShift action_98
action_212 (81) = happyShift action_99
action_212 (83) = happyShift action_100
action_212 (87) = happyShift action_52
action_212 (91) = happyShift action_53
action_212 (92) = happyShift action_54
action_212 (93) = happyShift action_55
action_212 (95) = happyShift action_56
action_212 (96) = happyShift action_57
action_212 (98) = happyShift action_101
action_212 (99) = happyShift action_102
action_212 (17) = happyGoto action_77
action_212 (20) = happyGoto action_78
action_212 (21) = happyGoto action_79
action_212 (23) = happyGoto action_80
action_212 (24) = happyGoto action_81
action_212 (26) = happyGoto action_82
action_212 (27) = happyGoto action_236
action_212 (28) = happyGoto action_104
action_212 (29) = happyGoto action_84
action_212 (30) = happyGoto action_85
action_212 (31) = happyGoto action_86
action_212 _ = happyFail

action_213 _ = happyReduce_129

action_214 (77) = happyShift action_235
action_214 _ = happyFail

action_215 (77) = happyShift action_234
action_215 _ = happyFail

action_216 (49) = happyShift action_38
action_216 (17) = happyGoto action_36
action_216 (34) = happyGoto action_226
action_216 (35) = happyGoto action_233
action_216 _ = happyFail

action_217 (49) = happyShift action_38
action_217 (17) = happyGoto action_36
action_217 (34) = happyGoto action_226
action_217 (35) = happyGoto action_227
action_217 (36) = happyGoto action_232
action_217 _ = happyReduce_103

action_218 _ = happyReduce_37

action_219 (82) = happyShift action_231
action_219 _ = happyFail

action_220 _ = happyReduce_137

action_221 (49) = happyShift action_38
action_221 (50) = happyShift action_87
action_221 (53) = happyShift action_88
action_221 (59) = happyShift action_89
action_221 (61) = happyShift action_90
action_221 (69) = happyShift action_91
action_221 (70) = happyShift action_92
action_221 (71) = happyShift action_93
action_221 (72) = happyShift action_94
action_221 (73) = happyShift action_95
action_221 (74) = happyShift action_96
action_221 (75) = happyShift action_97
action_221 (79) = happyShift action_98
action_221 (81) = happyShift action_99
action_221 (83) = happyShift action_100
action_221 (87) = happyShift action_52
action_221 (91) = happyShift action_53
action_221 (92) = happyShift action_54
action_221 (93) = happyShift action_55
action_221 (95) = happyShift action_56
action_221 (96) = happyShift action_57
action_221 (98) = happyShift action_101
action_221 (99) = happyShift action_102
action_221 (17) = happyGoto action_77
action_221 (20) = happyGoto action_78
action_221 (21) = happyGoto action_79
action_221 (23) = happyGoto action_80
action_221 (24) = happyGoto action_81
action_221 (26) = happyGoto action_82
action_221 (28) = happyGoto action_230
action_221 (29) = happyGoto action_84
action_221 (30) = happyGoto action_85
action_221 (31) = happyGoto action_86
action_221 _ = happyFail

action_222 (80) = happyShift action_229
action_222 _ = happyFail

action_223 (49) = happyShift action_38
action_223 (17) = happyGoto action_36
action_223 (34) = happyGoto action_226
action_223 (35) = happyGoto action_227
action_223 (36) = happyGoto action_228
action_223 _ = happyReduce_103

action_224 (49) = happyShift action_38
action_224 (50) = happyShift action_87
action_224 (53) = happyShift action_88
action_224 (59) = happyShift action_89
action_224 (61) = happyShift action_90
action_224 (69) = happyShift action_91
action_224 (70) = happyShift action_92
action_224 (71) = happyShift action_93
action_224 (72) = happyShift action_94
action_224 (73) = happyShift action_95
action_224 (74) = happyShift action_96
action_224 (75) = happyShift action_97
action_224 (79) = happyShift action_98
action_224 (81) = happyShift action_99
action_224 (83) = happyShift action_100
action_224 (87) = happyShift action_52
action_224 (91) = happyShift action_53
action_224 (92) = happyShift action_54
action_224 (93) = happyShift action_55
action_224 (95) = happyShift action_56
action_224 (96) = happyShift action_57
action_224 (98) = happyShift action_101
action_224 (99) = happyShift action_102
action_224 (17) = happyGoto action_77
action_224 (20) = happyGoto action_78
action_224 (21) = happyGoto action_79
action_224 (23) = happyGoto action_80
action_224 (24) = happyGoto action_81
action_224 (26) = happyGoto action_82
action_224 (28) = happyGoto action_225
action_224 (29) = happyGoto action_84
action_224 (30) = happyGoto action_85
action_224 (31) = happyGoto action_86
action_224 _ = happyFail

action_225 (82) = happyShift action_296
action_225 _ = happyFail

action_226 _ = happyReduce_99

action_227 _ = happyReduce_102

action_228 (78) = happyShift action_295
action_228 (85) = happyShift action_293
action_228 _ = happyFail

action_229 _ = happyReduce_134

action_230 (82) = happyShift action_294
action_230 _ = happyFail

action_231 _ = happyReduce_133

action_232 (78) = happyShift action_292
action_232 (85) = happyShift action_293
action_232 _ = happyFail

action_233 (78) = happyShift action_291
action_233 _ = happyFail

action_234 (49) = happyShift action_38
action_234 (17) = happyGoto action_36
action_234 (34) = happyGoto action_226
action_234 (35) = happyGoto action_227
action_234 (36) = happyGoto action_290
action_234 _ = happyReduce_103

action_235 (49) = happyShift action_38
action_235 (17) = happyGoto action_36
action_235 (34) = happyGoto action_226
action_235 (35) = happyGoto action_227
action_235 (36) = happyGoto action_289
action_235 _ = happyReduce_103

action_236 _ = happyReduce_109

action_237 _ = happyReduce_119

action_238 _ = happyReduce_118

action_239 (49) = happyShift action_38
action_239 (73) = happyShift action_209
action_239 (75) = happyShift action_287
action_239 (76) = happyShift action_288
action_239 (81) = happyShift action_210
action_239 (87) = happyShift action_211
action_239 (17) = happyGoto action_207
action_239 (41) = happyGoto action_286
action_239 _ = happyReduce_116

action_240 _ = happyReduce_125

action_241 (82) = happyShift action_285
action_241 _ = happyFail

action_242 (84) = happyShift action_284
action_242 _ = happyReduce_121

action_243 (49) = happyShift action_38
action_243 (73) = happyShift action_209
action_243 (81) = happyShift action_210
action_243 (87) = happyShift action_211
action_243 (17) = happyGoto action_207
action_243 (41) = happyGoto action_283
action_243 _ = happyReduce_122

action_244 _ = happyReduce_114

action_245 _ = happyReduce_117

action_246 _ = happyReduce_69

action_247 _ = happyReduce_46

action_248 (82) = happyShift action_282
action_248 _ = happyFail

action_249 (82) = happyShift action_281
action_249 _ = happyFail

action_250 (82) = happyShift action_280
action_250 _ = happyFail

action_251 (80) = happyShift action_279
action_251 _ = happyFail

action_252 (80) = happyShift action_278
action_252 _ = happyFail

action_253 (80) = happyShift action_277
action_253 _ = happyFail

action_254 (76) = happyShift action_276
action_254 _ = happyFail

action_255 _ = happyReduce_56

action_256 (86) = happyShift action_275
action_256 _ = happyFail

action_257 (62) = happyShift action_274
action_257 _ = happyFail

action_258 (49) = happyShift action_38
action_258 (50) = happyShift action_87
action_258 (53) = happyShift action_88
action_258 (59) = happyShift action_89
action_258 (61) = happyShift action_90
action_258 (69) = happyShift action_91
action_258 (70) = happyShift action_92
action_258 (71) = happyShift action_93
action_258 (72) = happyShift action_94
action_258 (73) = happyShift action_95
action_258 (74) = happyShift action_96
action_258 (75) = happyShift action_97
action_258 (79) = happyShift action_98
action_258 (81) = happyShift action_99
action_258 (83) = happyShift action_100
action_258 (87) = happyShift action_52
action_258 (91) = happyShift action_53
action_258 (92) = happyShift action_54
action_258 (93) = happyShift action_55
action_258 (95) = happyShift action_56
action_258 (96) = happyShift action_57
action_258 (98) = happyShift action_101
action_258 (99) = happyShift action_102
action_258 (17) = happyGoto action_77
action_258 (20) = happyGoto action_78
action_258 (21) = happyGoto action_79
action_258 (23) = happyGoto action_80
action_258 (24) = happyGoto action_81
action_258 (26) = happyGoto action_82
action_258 (28) = happyGoto action_273
action_258 (29) = happyGoto action_84
action_258 (30) = happyGoto action_85
action_258 (31) = happyGoto action_86
action_258 _ = happyFail

action_259 (49) = happyShift action_38
action_259 (50) = happyShift action_87
action_259 (53) = happyShift action_88
action_259 (59) = happyShift action_89
action_259 (61) = happyShift action_90
action_259 (69) = happyShift action_91
action_259 (70) = happyShift action_92
action_259 (71) = happyShift action_93
action_259 (72) = happyShift action_94
action_259 (73) = happyShift action_95
action_259 (74) = happyShift action_96
action_259 (75) = happyShift action_97
action_259 (79) = happyShift action_98
action_259 (81) = happyShift action_99
action_259 (83) = happyShift action_100
action_259 (87) = happyShift action_52
action_259 (91) = happyShift action_53
action_259 (92) = happyShift action_54
action_259 (93) = happyShift action_55
action_259 (95) = happyShift action_56
action_259 (96) = happyShift action_57
action_259 (98) = happyShift action_101
action_259 (99) = happyShift action_102
action_259 (17) = happyGoto action_77
action_259 (20) = happyGoto action_78
action_259 (21) = happyGoto action_79
action_259 (23) = happyGoto action_80
action_259 (24) = happyGoto action_81
action_259 (26) = happyGoto action_82
action_259 (28) = happyGoto action_272
action_259 (29) = happyGoto action_84
action_259 (30) = happyGoto action_85
action_259 (31) = happyGoto action_86
action_259 _ = happyFail

action_260 (78) = happyShift action_271
action_260 _ = happyFail

action_261 (85) = happyShift action_269
action_261 (88) = happyShift action_270
action_261 _ = happyReduce_107

action_262 (49) = happyShift action_38
action_262 (17) = happyGoto action_140
action_262 (18) = happyGoto action_184
action_262 (32) = happyGoto action_268
action_262 (33) = happyGoto action_186
action_262 _ = happyReduce_96

action_263 _ = happyReduce_93

action_264 (49) = happyShift action_38
action_264 (50) = happyShift action_87
action_264 (53) = happyShift action_88
action_264 (59) = happyShift action_89
action_264 (61) = happyShift action_90
action_264 (69) = happyShift action_91
action_264 (70) = happyShift action_92
action_264 (71) = happyShift action_93
action_264 (72) = happyShift action_94
action_264 (73) = happyShift action_95
action_264 (74) = happyShift action_96
action_264 (75) = happyShift action_97
action_264 (79) = happyShift action_98
action_264 (81) = happyShift action_99
action_264 (83) = happyShift action_100
action_264 (87) = happyShift action_52
action_264 (91) = happyShift action_53
action_264 (92) = happyShift action_54
action_264 (93) = happyShift action_55
action_264 (95) = happyShift action_56
action_264 (96) = happyShift action_57
action_264 (98) = happyShift action_101
action_264 (99) = happyShift action_102
action_264 (17) = happyGoto action_77
action_264 (20) = happyGoto action_78
action_264 (21) = happyGoto action_79
action_264 (23) = happyGoto action_80
action_264 (24) = happyGoto action_81
action_264 (26) = happyGoto action_82
action_264 (27) = happyGoto action_267
action_264 (28) = happyGoto action_104
action_264 (29) = happyGoto action_84
action_264 (30) = happyGoto action_85
action_264 (31) = happyGoto action_86
action_264 _ = happyFail

action_265 (49) = happyShift action_38
action_265 (50) = happyShift action_87
action_265 (53) = happyShift action_88
action_265 (59) = happyShift action_89
action_265 (61) = happyShift action_90
action_265 (69) = happyShift action_91
action_265 (70) = happyShift action_92
action_265 (71) = happyShift action_93
action_265 (72) = happyShift action_94
action_265 (73) = happyShift action_95
action_265 (74) = happyShift action_96
action_265 (75) = happyShift action_97
action_265 (79) = happyShift action_98
action_265 (81) = happyShift action_99
action_265 (83) = happyShift action_100
action_265 (87) = happyShift action_52
action_265 (91) = happyShift action_53
action_265 (92) = happyShift action_54
action_265 (93) = happyShift action_55
action_265 (95) = happyShift action_56
action_265 (96) = happyShift action_57
action_265 (98) = happyShift action_101
action_265 (99) = happyShift action_102
action_265 (17) = happyGoto action_77
action_265 (20) = happyGoto action_78
action_265 (21) = happyGoto action_79
action_265 (23) = happyGoto action_80
action_265 (24) = happyGoto action_81
action_265 (26) = happyGoto action_82
action_265 (28) = happyGoto action_266
action_265 (29) = happyGoto action_84
action_265 (30) = happyGoto action_85
action_265 (31) = happyGoto action_86
action_265 _ = happyFail

action_266 (82) = happyShift action_313
action_266 _ = happyFail

action_267 _ = happyReduce_97

action_268 _ = happyReduce_94

action_269 (49) = happyShift action_38
action_269 (73) = happyShift action_209
action_269 (81) = happyShift action_210
action_269 (87) = happyShift action_211
action_269 (17) = happyGoto action_207
action_269 (37) = happyGoto action_312
action_269 (41) = happyGoto action_261
action_269 _ = happyReduce_108

action_270 (49) = happyShift action_38
action_270 (50) = happyShift action_87
action_270 (53) = happyShift action_88
action_270 (59) = happyShift action_89
action_270 (61) = happyShift action_90
action_270 (69) = happyShift action_91
action_270 (70) = happyShift action_92
action_270 (71) = happyShift action_93
action_270 (72) = happyShift action_94
action_270 (73) = happyShift action_95
action_270 (74) = happyShift action_96
action_270 (75) = happyShift action_97
action_270 (79) = happyShift action_98
action_270 (81) = happyShift action_99
action_270 (83) = happyShift action_100
action_270 (87) = happyShift action_52
action_270 (91) = happyShift action_53
action_270 (92) = happyShift action_54
action_270 (93) = happyShift action_55
action_270 (95) = happyShift action_56
action_270 (96) = happyShift action_57
action_270 (98) = happyShift action_101
action_270 (99) = happyShift action_102
action_270 (17) = happyGoto action_77
action_270 (20) = happyGoto action_78
action_270 (21) = happyGoto action_79
action_270 (23) = happyGoto action_80
action_270 (24) = happyGoto action_81
action_270 (26) = happyGoto action_82
action_270 (27) = happyGoto action_311
action_270 (28) = happyGoto action_104
action_270 (29) = happyGoto action_84
action_270 (30) = happyGoto action_85
action_270 (31) = happyGoto action_86
action_270 _ = happyFail

action_271 _ = happyReduce_71

action_272 (82) = happyShift action_310
action_272 _ = happyFail

action_273 (80) = happyShift action_309
action_273 _ = happyFail

action_274 (49) = happyShift action_38
action_274 (50) = happyShift action_87
action_274 (53) = happyShift action_88
action_274 (59) = happyShift action_89
action_274 (61) = happyShift action_90
action_274 (69) = happyShift action_91
action_274 (70) = happyShift action_92
action_274 (71) = happyShift action_93
action_274 (72) = happyShift action_94
action_274 (73) = happyShift action_95
action_274 (74) = happyShift action_96
action_274 (75) = happyShift action_97
action_274 (79) = happyShift action_98
action_274 (81) = happyShift action_99
action_274 (83) = happyShift action_100
action_274 (87) = happyShift action_52
action_274 (91) = happyShift action_53
action_274 (92) = happyShift action_54
action_274 (93) = happyShift action_55
action_274 (95) = happyShift action_56
action_274 (96) = happyShift action_57
action_274 (98) = happyShift action_101
action_274 (99) = happyShift action_102
action_274 (17) = happyGoto action_77
action_274 (20) = happyGoto action_78
action_274 (21) = happyGoto action_79
action_274 (23) = happyGoto action_80
action_274 (24) = happyGoto action_81
action_274 (26) = happyGoto action_82
action_274 (27) = happyGoto action_308
action_274 (28) = happyGoto action_104
action_274 (29) = happyGoto action_84
action_274 (30) = happyGoto action_85
action_274 (31) = happyGoto action_86
action_274 _ = happyFail

action_275 (49) = happyShift action_38
action_275 (50) = happyShift action_87
action_275 (53) = happyShift action_88
action_275 (59) = happyShift action_89
action_275 (61) = happyShift action_90
action_275 (69) = happyShift action_91
action_275 (70) = happyShift action_92
action_275 (71) = happyShift action_93
action_275 (72) = happyShift action_94
action_275 (73) = happyShift action_95
action_275 (74) = happyShift action_96
action_275 (75) = happyShift action_97
action_275 (79) = happyShift action_98
action_275 (81) = happyShift action_99
action_275 (83) = happyShift action_100
action_275 (87) = happyShift action_52
action_275 (91) = happyShift action_53
action_275 (92) = happyShift action_54
action_275 (93) = happyShift action_55
action_275 (95) = happyShift action_56
action_275 (96) = happyShift action_57
action_275 (98) = happyShift action_101
action_275 (99) = happyShift action_102
action_275 (17) = happyGoto action_77
action_275 (20) = happyGoto action_78
action_275 (21) = happyGoto action_79
action_275 (23) = happyGoto action_80
action_275 (24) = happyGoto action_81
action_275 (26) = happyGoto action_82
action_275 (28) = happyGoto action_307
action_275 (29) = happyGoto action_84
action_275 (30) = happyGoto action_85
action_275 (31) = happyGoto action_86
action_275 _ = happyFail

action_276 _ = happyReduce_88

action_277 _ = happyReduce_50

action_278 _ = happyReduce_53

action_279 _ = happyReduce_55

action_280 _ = happyReduce_49

action_281 _ = happyReduce_52

action_282 _ = happyReduce_54

action_283 _ = happyReduce_127

action_284 (49) = happyShift action_38
action_284 (73) = happyShift action_209
action_284 (81) = happyShift action_210
action_284 (87) = happyShift action_211
action_284 (17) = happyGoto action_239
action_284 (41) = happyGoto action_240
action_284 (42) = happyGoto action_306
action_284 (43) = happyGoto action_242
action_284 (44) = happyGoto action_243
action_284 _ = happyFail

action_285 _ = happyReduce_115

action_286 _ = happyReduce_126

action_287 (49) = happyShift action_38
action_287 (17) = happyGoto action_305
action_287 _ = happyFail

action_288 (49) = happyShift action_38
action_288 (17) = happyGoto action_304
action_288 _ = happyFail

action_289 (78) = happyShift action_303
action_289 (85) = happyShift action_293
action_289 _ = happyFail

action_290 (78) = happyShift action_302
action_290 (85) = happyShift action_293
action_290 _ = happyFail

action_291 (55) = happyShift action_298
action_291 (16) = happyGoto action_301
action_291 _ = happyReduce_31

action_292 (55) = happyShift action_298
action_292 (16) = happyGoto action_300
action_292 _ = happyReduce_31

action_293 (49) = happyShift action_38
action_293 (17) = happyGoto action_36
action_293 (34) = happyGoto action_226
action_293 (35) = happyGoto action_299
action_293 _ = happyReduce_101

action_294 _ = happyReduce_136

action_295 (55) = happyShift action_298
action_295 (16) = happyGoto action_297
action_295 _ = happyReduce_31

action_296 _ = happyReduce_135

action_297 _ = happyReduce_21

action_298 (49) = happyShift action_38
action_298 (17) = happyGoto action_115
action_298 (19) = happyGoto action_318
action_298 _ = happyFail

action_299 _ = happyReduce_100

action_300 _ = happyReduce_23

action_301 _ = happyReduce_25

action_302 (55) = happyShift action_298
action_302 (16) = happyGoto action_317
action_302 _ = happyReduce_31

action_303 (55) = happyShift action_298
action_303 (16) = happyGoto action_316
action_303 _ = happyReduce_31

action_304 _ = happyReduce_123

action_305 _ = happyReduce_124

action_306 _ = happyReduce_120

action_307 (82) = happyShift action_315
action_307 _ = happyFail

action_308 _ = happyReduce_70

action_309 _ = happyReduce_58

action_310 _ = happyReduce_57

action_311 (85) = happyShift action_314
action_311 _ = happyReduce_105

action_312 _ = happyReduce_106

action_313 (97) = happyReduce_51
action_313 _ = happyReduce_51

action_314 (49) = happyShift action_38
action_314 (73) = happyShift action_209
action_314 (81) = happyShift action_210
action_314 (87) = happyShift action_211
action_314 (17) = happyGoto action_207
action_314 (37) = happyGoto action_319
action_314 (41) = happyGoto action_261
action_314 _ = happyReduce_108

action_315 _ = happyReduce_59

action_316 _ = happyReduce_22

action_317 _ = happyReduce_24

action_318 _ = happyReduce_32

action_319 _ = happyReduce_104

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
	(HappyAbsSyn36  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.DataDecl happy_var_2 A.NotSized A.Ind happy_var_3 happy_var_5 (reverse happy_var_7) happy_var_9
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 10 8 happyReduction_22
happyReduction_22 ((HappyAbsSyn16  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_4) `HappyStk`
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
	(HappyAbsSyn36  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.DataDecl happy_var_2 A.NotSized A.CoInd happy_var_3 happy_var_5 (reverse happy_var_7) happy_var_9
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 10 10 happyReduction_24
happyReduction_24 ((HappyAbsSyn16  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_4) `HappyStk`
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
	(HappyAbsSyn35  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.RecordDecl happy_var_2 happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 12 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.FunDecl A.Ind happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 5 13 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
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
happyReduction_29 ((HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.LetDecl False happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 5 15 happyReduction_30
happyReduction_30 ((HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_3) `HappyStk`
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
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  22 happyReduction_46
happyReduction_46 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
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

happyReduce_49 = happyReduce 5 24 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBind (Dec Default) {- A.defaultDec -} happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 5 24 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBind A.irrelevantDec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 6 24 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBind (Dec happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_52 = happyReduce 5 24 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBounded A.defaultDec    happy_var_2 A.Lt happy_var_4
	) `HappyStk` happyRest

happyReduce_53 = happyReduce 5 24 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBounded A.irrelevantDec happy_var_2 A.Lt happy_var_4
	) `HappyStk` happyRest

happyReduce_54 = happyReduce 5 24 happyReduction_54
happyReduction_54 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBounded A.defaultDec    happy_var_2 A.Le happy_var_4
	) `HappyStk` happyRest

happyReduce_55 = happyReduce 5 24 happyReduction_55
happyReduction_55 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBounded A.irrelevantDec happy_var_2 A.Le happy_var_4
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_3  25 happyReduction_56
happyReduction_56 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn24
		 (C.TBind A.defaultDec [happy_var_1] happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happyReduce 5 25 happyReduction_57
happyReduction_57 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBind A.defaultDec [happy_var_2] happy_var_4
	) `HappyStk` happyRest

happyReduce_58 = happyReduce 5 25 happyReduction_58
happyReduction_58 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBind A.irrelevantDec [happy_var_2] happy_var_4
	) `HappyStk` happyRest

happyReduce_59 = happyReduce 6 25 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBind (Dec happy_var_1) [happy_var_3] happy_var_5
	) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_1  26 happyReduction_60
happyReduction_60 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn24
		 (C.TBind (Dec Default) {- A.defaultDec -} [] happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  26 happyReduction_61
happyReduction_61 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (C.TBind A.irrelevantDec [] happy_var_2
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  26 happyReduction_62
happyReduction_62 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn24
		 (C.TBind (Dec happy_var_1) [] happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  26 happyReduction_63
happyReduction_63 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  26 happyReduction_64
happyReduction_64 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn24
		 (C.TMeasure happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  26 happyReduction_65
happyReduction_65 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn24
		 (C.TBound happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  27 happyReduction_66
happyReduction_66 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  27 happyReduction_67
happyReduction_67 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (C.Pair happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  28 happyReduction_68
happyReduction_68 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn27
		 (C.Quant A.Pi happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happyReduce 4 28 happyReduction_69
happyReduction_69 ((HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (foldr C.Lam happy_var_4 happy_var_2
	) `HappyStk` happyRest

happyReduce_70 = happyReduce 6 28 happyReduction_70
happyReduction_70 ((HappyAbsSyn27  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (C.LLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_71 = happyReduce 5 28 happyReduction_71
happyReduction_71 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (C.Case happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_72 = happySpecReduce_1  28 happyReduction_72
happyReduction_72 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  28 happyReduction_73
happyReduction_73 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (C.Plus happy_var_1 happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  29 happyReduction_74
happyReduction_74 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn27
		 (let (f : args) = reverse happy_var_1 in
                if null args then f else C.App f args
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_2  29 happyReduction_75
happyReduction_75 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (C.CoSet happy_var_2
	)
happyReduction_75 _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  29 happyReduction_76
happyReduction_76 _
	 =  HappyAbsSyn27
		 (C.Set C.Zero
	)

happyReduce_77 = happySpecReduce_2  29 happyReduction_77
happyReduction_77 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (C.Set happy_var_2
	)
happyReduction_77 _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  29 happyReduction_78
happyReduction_78 (HappyAbsSyn27  happy_var_3)
	_
	(HappyTerminal (T.Number happy_var_1 _))
	 =  HappyAbsSyn27
		 (let n = read happy_var_1 in
                                            if n==0 then C.Zero else
                                            iterate (C.Plus happy_var_3) happy_var_3 !! (n-1)
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  29 happyReduction_79
happyReduction_79 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn27
		 (C.Quant A.Sigma happy_var_1 happy_var_3
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  30 happyReduction_80
happyReduction_80 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_2  30 happyReduction_81
happyReduction_81 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_1
	)
happyReduction_81 _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  30 happyReduction_82
happyReduction_82 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (C.Proj happy_var_3 : happy_var_1
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_2  30 happyReduction_83
happyReduction_83 _
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (C.Set C.Zero : happy_var_1
	)
happyReduction_83 _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  31 happyReduction_84
happyReduction_84 _
	 =  HappyAbsSyn27
		 (C.Size
	)

happyReduce_85 = happySpecReduce_1  31 happyReduction_85
happyReduction_85 _
	 =  HappyAbsSyn27
		 (C.Max
	)

happyReduce_86 = happySpecReduce_1  31 happyReduction_86
happyReduction_86 _
	 =  HappyAbsSyn27
		 (C.Infty
	)

happyReduce_87 = happySpecReduce_1  31 happyReduction_87
happyReduction_87 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn27
		 (C.Ident happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happyReduce 5 31 happyReduction_88
happyReduction_88 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (C.Sing happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_89 = happySpecReduce_3  31 happyReduction_89
happyReduction_89 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (happy_var_2
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  31 happyReduction_90
happyReduction_90 _
	 =  HappyAbsSyn27
		 (C.Unknown
	)

happyReduce_91 = happySpecReduce_2  31 happyReduction_91
happyReduction_91 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (C.Succ happy_var_2
	)
happyReduction_91 _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  31 happyReduction_92
happyReduction_92 (HappyTerminal (T.Number happy_var_1 _))
	 =  HappyAbsSyn27
		 (iterate C.Succ C.Zero !! (read happy_var_1)
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happyReduce 4 31 happyReduction_93
happyReduction_93 (_ `HappyStk`
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (C.Record happy_var_3
	) `HappyStk` happyRest

happyReduce_94 = happySpecReduce_3  32 happyReduction_94
happyReduction_94 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1 : happy_var_3
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  32 happyReduction_95
happyReduction_95 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn32
		 ([happy_var_1]
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_0  32 happyReduction_96
happyReduction_96  =  HappyAbsSyn32
		 ([]
	)

happyReduce_97 = happySpecReduce_3  33 happyReduction_97
happyReduction_97 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn33
		 ((happy_var_1,happy_var_3)
	)
happyReduction_97 _ _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_3  34 happyReduction_98
happyReduction_98 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn34
		 (C.TypeSig happy_var_1 happy_var_3
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  35 happyReduction_99
happyReduction_99 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_3  36 happyReduction_100
happyReduction_100 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_3 : happy_var_1
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_2  36 happyReduction_101
happyReduction_101 _
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1
	)
happyReduction_101 _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  36 happyReduction_102
happyReduction_102 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn36
		 ([happy_var_1]
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_0  36 happyReduction_103
happyReduction_103  =  HappyAbsSyn36
		 ([]
	)

happyReduce_104 = happyReduce 5 37 happyReduction_104
happyReduction_104 ((HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 ((C.Clause Nothing [happy_var_1] (Just happy_var_3)) : happy_var_5
	) `HappyStk` happyRest

happyReduce_105 = happySpecReduce_3  37 happyReduction_105
happyReduction_105 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn37
		 ((C.Clause Nothing [happy_var_1] (Just happy_var_3)) : []
	)
happyReduction_105 _ _ _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_3  37 happyReduction_106
happyReduction_106 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn37
		 ((C.Clause Nothing [happy_var_1] Nothing) : happy_var_3
	)
happyReduction_106 _ _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_1  37 happyReduction_107
happyReduction_107 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn37
		 ((C.Clause Nothing [happy_var_1] Nothing) : []
	)
happyReduction_107 _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_0  37 happyReduction_108
happyReduction_108  =  HappyAbsSyn37
		 ([]
	)

happyReduce_109 = happyReduce 4 38 happyReduction_109
happyReduction_109 ((HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_2) `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (C.Clause (Just happy_var_1) happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_110 = happySpecReduce_2  38 happyReduction_110
happyReduction_110 (HappyAbsSyn39  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn38
		 (C.Clause (Just happy_var_1) happy_var_2 Nothing
	)
happyReduction_110 _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  39 happyReduction_111
happyReduction_111 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (reverse happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_0  40 happyReduction_112
happyReduction_112  =  HappyAbsSyn39
		 ([]
	)

happyReduce_113 = happySpecReduce_2  40 happyReduction_113
happyReduction_113 (HappyAbsSyn41  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_2 : happy_var_1
	)
happyReduction_113 _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_2  41 happyReduction_114
happyReduction_114 _
	_
	 =  HappyAbsSyn41
		 (C.AbsurdP
	)

happyReduce_115 = happySpecReduce_3  41 happyReduction_115
happyReduction_115 _
	(HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (happy_var_2
	)
happyReduction_115 _ _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  41 happyReduction_116
happyReduction_116 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn41
		 (C.IdentP happy_var_1
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_2  41 happyReduction_117
happyReduction_117 (HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (C.SuccP happy_var_2
	)
happyReduction_117 _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_2  41 happyReduction_118
happyReduction_118 _
	_
	 =  HappyAbsSyn41
		 (C.DotP (C.Set C.Zero)
	)

happyReduce_119 = happySpecReduce_2  41 happyReduction_119
happyReduction_119 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (C.DotP happy_var_2
	)
happyReduction_119 _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_3  42 happyReduction_120
happyReduction_120 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (C.PairP happy_var_1 happy_var_3
	)
happyReduction_120 _ _ _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_1  42 happyReduction_121
happyReduction_121 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_121 _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  43 happyReduction_122
happyReduction_122 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn41
		 (let (c, ps) = happy_var_1 in C.ConP c (reverse ps)
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_3  43 happyReduction_123
happyReduction_123 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn41
		 (C.SizeP happy_var_1 happy_var_3
	)
happyReduction_123 _ _ _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_3  43 happyReduction_124
happyReduction_124 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn41
		 (C.SizeP happy_var_3 happy_var_1
	)
happyReduction_124 _ _ _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_1  43 happyReduction_125
happyReduction_125 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_2  44 happyReduction_126
happyReduction_126 (HappyAbsSyn41  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn44
		 ((happy_var_1, [happy_var_2])
	)
happyReduction_126 _ _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_2  44 happyReduction_127
happyReduction_127 (HappyAbsSyn41  happy_var_2)
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn44
		 (let (c, ps) = happy_var_1 in (c, happy_var_2 : ps)
	)
happyReduction_127 _ _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_1  45 happyReduction_128
happyReduction_128 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn37
		 (reverse happy_var_1
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_3  46 happyReduction_129
happyReduction_129 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_3 : happy_var_1
	)
happyReduction_129 _ _ _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_2  46 happyReduction_130
happyReduction_130 _
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_1
	)
happyReduction_130 _ _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_1  46 happyReduction_131
happyReduction_131 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn46
		 ([happy_var_1]
	)
happyReduction_131 _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_0  46 happyReduction_132
happyReduction_132  =  HappyAbsSyn46
		 ([]
	)

happyReduce_133 = happyReduce 5 47 happyReduction_133
happyReduction_133 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBind A.paramDec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_134 = happyReduce 5 47 happyReduction_134
happyReduction_134 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBind A.irrelevantDec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_135 = happyReduce 6 47 happyReduction_135
happyReduction_135 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBind (Dec happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_136 = happyReduce 6 47 happyReduction_136
happyReduction_136 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TBind (Dec SPos) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_137 = happyReduce 4 47 happyReduction_137
happyReduction_137 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (C.TSized happy_var_3
	) `HappyStk` happyRest

happyReduce_138 = happySpecReduce_0  48 happyReduction_138
happyReduction_138  =  HappyAbsSyn48
		 ([]
	)

happyReduce_139 = happySpecReduce_2  48 happyReduction_139
happyReduction_139 (HappyAbsSyn48  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1 : happy_var_2
	)
happyReduction_139 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 100 100 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T.Id happy_dollar_dollar _ -> cont 49;
	T.Number happy_dollar_dollar _ -> cont 50;
	T.Data _ -> cont 51;
	T.CoData _ -> cont 52;
	T.Record _ -> cont 53;
	T.Sized _ -> cont 54;
	T.Fields _ -> cont 55;
	T.Mutual _ -> cont 56;
	T.Fun _ -> cont 57;
	T.CoFun _ -> cont 58;
	T.Case _ -> cont 59;
	T.Def _ -> cont 60;
	T.Let _ -> cont 61;
	T.In _ -> cont 62;
	T.Eval _ -> cont 63;
	T.Fail _ -> cont 64;
	T.Check _ -> cont 65;
	T.TrustMe _ -> cont 66;
	T.Impredicative _ -> cont 67;
	T.Type _ -> cont 68;
	T.Set _ -> cont 69;
	T.CoSet _ -> cont 70;
	T.Size _ -> cont 71;
	T.Infty _ -> cont 72;
	T.Succ _ -> cont 73;
	T.Max _ -> cont 74;
	T.AngleOpen _ -> cont 75;
	T.AngleClose _ -> cont 76;
	T.BrOpen _ -> cont 77;
	T.BrClose _ -> cont 78;
	T.BracketOpen _ -> cont 79;
	T.BracketClose _ -> cont 80;
	T.PrOpen _ -> cont 81;
	T.PrClose _ -> cont 82;
	T.Bar _ -> cont 83;
	T.Comma _ -> cont 84;
	T.Sem _ -> cont 85;
	T.Col _ -> cont 86;
	T.Dot _ -> cont 87;
	T.Arrow _ -> cont 88;
	T.Leq _ -> cont 89;
	T.Eq _ -> cont 90;
	T.PlusPlus _ -> cont 91;
	T.Plus _ -> cont 92;
	T.Minus _ -> cont 93;
	T.Slash _ -> cont 94;
	T.Times _ -> cont 95;
	T.Hat _ -> cont 96;
	T.Amp _ -> cont 97;
	T.Lam _ -> cont 98;
	T.Underscore _ -> cont 99;
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
