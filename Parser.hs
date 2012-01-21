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
	| HappyAbsSyn36 ([([Name],C.Expr)])
	| HappyAbsSyn37 (([Name],C.Expr))
	| HappyAbsSyn38 (C.TypeSig)
	| HappyAbsSyn39 (C.Constructor)
	| HappyAbsSyn40 ([C.Constructor ])
	| HappyAbsSyn41 ([C.Clause])
	| HappyAbsSyn42 (C.Clause)
	| HappyAbsSyn43 ([C.Pattern])
	| HappyAbsSyn45 (C.Pattern)
	| HappyAbsSyn48 ((Name, [C.Pattern]))
	| HappyAbsSyn50 ([C.Clause ])
	| HappyAbsSyn52 (C.Telescope)

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
 action_321,
 action_322,
 action_323,
 action_324,
 action_325,
 action_326,
 action_327,
 action_328,
 action_329,
 action_330,
 action_331,
 action_332,
 action_333,
 action_334,
 action_335,
 action_336,
 action_337 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148 :: () => ({-HappyReduction (HappyIdentity) = -}
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

action_2 (55) = happyShift action_14
action_2 (56) = happyShift action_15
action_2 (57) = happyShift action_16
action_2 (58) = happyShift action_17
action_2 (60) = happyShift action_18
action_2 (61) = happyShift action_19
action_2 (62) = happyShift action_20
action_2 (65) = happyShift action_21
action_2 (67) = happyShift action_22
action_2 (68) = happyShift action_23
action_2 (69) = happyShift action_24
action_2 (70) = happyShift action_25
action_2 (71) = happyShift action_26
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

action_3 (104) = happyAccept
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

action_14 (53) = happyShift action_38
action_14 (17) = happyGoto action_46
action_14 _ = happyFail

action_15 (53) = happyShift action_38
action_15 (17) = happyGoto action_45
action_15 _ = happyFail

action_16 (53) = happyShift action_38
action_16 (17) = happyGoto action_44
action_16 _ = happyFail

action_17 (55) = happyShift action_42
action_17 (56) = happyShift action_43
action_17 _ = happyFail

action_18 (81) = happyShift action_41
action_18 _ = happyFail

action_19 (53) = happyShift action_38
action_19 (17) = happyGoto action_36
action_19 (38) = happyGoto action_40
action_19 _ = happyFail

action_20 (53) = happyShift action_38
action_20 (17) = happyGoto action_36
action_20 (38) = happyGoto action_39
action_20 _ = happyFail

action_21 (53) = happyShift action_38
action_21 (17) = happyGoto action_36
action_21 (38) = happyGoto action_37
action_21 _ = happyFail

action_22 (65) = happyShift action_35
action_22 _ = happyFail

action_23 (55) = happyShift action_14
action_23 (56) = happyShift action_15
action_23 (57) = happyShift action_16
action_23 (58) = happyShift action_17
action_23 (60) = happyShift action_18
action_23 (61) = happyShift action_19
action_23 (62) = happyShift action_20
action_23 (65) = happyShift action_21
action_23 (67) = happyShift action_22
action_23 (68) = happyShift action_23
action_23 (69) = happyShift action_24
action_23 (70) = happyShift action_25
action_23 (71) = happyShift action_26
action_23 (81) = happyShift action_34
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

action_24 (55) = happyShift action_14
action_24 (56) = happyShift action_15
action_24 (57) = happyShift action_16
action_24 (58) = happyShift action_17
action_24 (60) = happyShift action_18
action_24 (61) = happyShift action_19
action_24 (62) = happyShift action_20
action_24 (65) = happyShift action_21
action_24 (67) = happyShift action_22
action_24 (68) = happyShift action_23
action_24 (69) = happyShift action_24
action_24 (70) = happyShift action_25
action_24 (71) = happyShift action_26
action_24 (81) = happyShift action_32
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

action_25 (55) = happyShift action_14
action_25 (56) = happyShift action_15
action_25 (57) = happyShift action_16
action_25 (58) = happyShift action_17
action_25 (60) = happyShift action_18
action_25 (61) = happyShift action_19
action_25 (62) = happyShift action_20
action_25 (65) = happyShift action_21
action_25 (67) = happyShift action_22
action_25 (68) = happyShift action_23
action_25 (69) = happyShift action_24
action_25 (70) = happyShift action_25
action_25 (71) = happyShift action_26
action_25 (81) = happyShift action_30
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

action_26 (55) = happyShift action_14
action_26 (56) = happyShift action_15
action_26 (57) = happyShift action_16
action_26 (58) = happyShift action_17
action_26 (60) = happyShift action_18
action_26 (61) = happyShift action_19
action_26 (62) = happyShift action_20
action_26 (65) = happyShift action_21
action_26 (67) = happyShift action_22
action_26 (68) = happyShift action_23
action_26 (69) = happyShift action_24
action_26 (70) = happyShift action_25
action_26 (71) = happyShift action_26
action_26 (81) = happyShift action_28
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

action_35 (53) = happyShift action_38
action_35 (17) = happyGoto action_36
action_35 (38) = happyGoto action_67
action_35 _ = happyFail

action_36 (90) = happyShift action_66
action_36 _ = happyFail

action_37 (94) = happyShift action_65
action_37 _ = happyFail

action_38 _ = happyReduce_33

action_39 (81) = happyShift action_64
action_39 _ = happyFail

action_40 (81) = happyShift action_63
action_40 _ = happyFail

action_41 (5) = happyGoto action_62
action_41 _ = happyReduce_2

action_42 (53) = happyShift action_38
action_42 (17) = happyGoto action_61
action_42 _ = happyFail

action_43 (53) = happyShift action_38
action_43 (17) = happyGoto action_60
action_43 _ = happyFail

action_44 (83) = happyShift action_50
action_44 (85) = happyShift action_51
action_44 (91) = happyShift action_52
action_44 (95) = happyShift action_53
action_44 (96) = happyShift action_54
action_44 (97) = happyShift action_55
action_44 (99) = happyShift action_56
action_44 (100) = happyShift action_57
action_44 (20) = happyGoto action_47
action_44 (51) = happyGoto action_48
action_44 (52) = happyGoto action_59
action_44 _ = happyReduce_147

action_45 (83) = happyShift action_50
action_45 (85) = happyShift action_51
action_45 (91) = happyShift action_52
action_45 (95) = happyShift action_53
action_45 (96) = happyShift action_54
action_45 (97) = happyShift action_55
action_45 (99) = happyShift action_56
action_45 (100) = happyShift action_57
action_45 (20) = happyGoto action_47
action_45 (51) = happyGoto action_48
action_45 (52) = happyGoto action_58
action_45 _ = happyReduce_147

action_46 (83) = happyShift action_50
action_46 (85) = happyShift action_51
action_46 (91) = happyShift action_52
action_46 (95) = happyShift action_53
action_46 (96) = happyShift action_54
action_46 (97) = happyShift action_55
action_46 (99) = happyShift action_56
action_46 (100) = happyShift action_57
action_46 (20) = happyGoto action_47
action_46 (51) = happyGoto action_48
action_46 (52) = happyGoto action_49
action_46 _ = happyReduce_147

action_47 (85) = happyShift action_125
action_47 _ = happyFail

action_48 (83) = happyShift action_50
action_48 (85) = happyShift action_51
action_48 (91) = happyShift action_52
action_48 (95) = happyShift action_53
action_48 (96) = happyShift action_54
action_48 (97) = happyShift action_55
action_48 (99) = happyShift action_56
action_48 (100) = happyShift action_57
action_48 (20) = happyGoto action_47
action_48 (51) = happyGoto action_48
action_48 (52) = happyGoto action_124
action_48 _ = happyReduce_147

action_49 (90) = happyShift action_123
action_49 _ = happyFail

action_50 (53) = happyShift action_38
action_50 (17) = happyGoto action_118
action_50 (19) = happyGoto action_122
action_50 _ = happyFail

action_51 (53) = happyShift action_38
action_51 (58) = happyShift action_120
action_51 (96) = happyShift action_121
action_51 (17) = happyGoto action_118
action_51 (19) = happyGoto action_119
action_51 _ = happyFail

action_52 _ = happyReduce_41

action_53 _ = happyReduce_38

action_54 _ = happyReduce_39

action_55 _ = happyReduce_40

action_56 _ = happyReduce_43

action_57 _ = happyReduce_42

action_58 (90) = happyShift action_117
action_58 _ = happyFail

action_59 (90) = happyShift action_116
action_59 _ = happyFail

action_60 (83) = happyShift action_50
action_60 (85) = happyShift action_51
action_60 (91) = happyShift action_52
action_60 (95) = happyShift action_53
action_60 (96) = happyShift action_54
action_60 (97) = happyShift action_55
action_60 (99) = happyShift action_56
action_60 (100) = happyShift action_57
action_60 (20) = happyGoto action_47
action_60 (51) = happyGoto action_48
action_60 (52) = happyGoto action_115
action_60 _ = happyReduce_147

action_61 (83) = happyShift action_50
action_61 (85) = happyShift action_51
action_61 (91) = happyShift action_52
action_61 (95) = happyShift action_53
action_61 (96) = happyShift action_54
action_61 (97) = happyShift action_55
action_61 (99) = happyShift action_56
action_61 (100) = happyShift action_57
action_61 (20) = happyGoto action_47
action_61 (51) = happyGoto action_48
action_61 (52) = happyGoto action_114
action_61 _ = happyReduce_147

action_62 (55) = happyShift action_14
action_62 (56) = happyShift action_15
action_62 (57) = happyShift action_16
action_62 (58) = happyShift action_17
action_62 (60) = happyShift action_18
action_62 (61) = happyShift action_19
action_62 (62) = happyShift action_20
action_62 (65) = happyShift action_21
action_62 (67) = happyShift action_22
action_62 (68) = happyShift action_23
action_62 (69) = happyShift action_24
action_62 (70) = happyShift action_25
action_62 (71) = happyShift action_26
action_62 (82) = happyShift action_113
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

action_63 (53) = happyShift action_38
action_63 (17) = happyGoto action_108
action_63 (42) = happyGoto action_109
action_63 (49) = happyGoto action_112
action_63 (50) = happyGoto action_111
action_63 _ = happyReduce_141

action_64 (53) = happyShift action_38
action_64 (17) = happyGoto action_108
action_64 (42) = happyGoto action_109
action_64 (49) = happyGoto action_110
action_64 (50) = happyGoto action_111
action_64 _ = happyReduce_141

action_65 (53) = happyShift action_38
action_65 (54) = happyShift action_89
action_65 (57) = happyShift action_90
action_65 (63) = happyShift action_91
action_65 (65) = happyShift action_92
action_65 (73) = happyShift action_93
action_65 (74) = happyShift action_94
action_65 (75) = happyShift action_95
action_65 (76) = happyShift action_96
action_65 (77) = happyShift action_97
action_65 (78) = happyShift action_98
action_65 (79) = happyShift action_99
action_65 (83) = happyShift action_100
action_65 (85) = happyShift action_101
action_65 (87) = happyShift action_102
action_65 (91) = happyShift action_52
action_65 (95) = happyShift action_53
action_65 (96) = happyShift action_54
action_65 (97) = happyShift action_55
action_65 (99) = happyShift action_56
action_65 (100) = happyShift action_57
action_65 (102) = happyShift action_103
action_65 (103) = happyShift action_104
action_65 (17) = happyGoto action_77
action_65 (20) = happyGoto action_78
action_65 (21) = happyGoto action_79
action_65 (23) = happyGoto action_80
action_65 (25) = happyGoto action_81
action_65 (27) = happyGoto action_82
action_65 (28) = happyGoto action_105
action_65 (29) = happyGoto action_106
action_65 (30) = happyGoto action_107
action_65 (31) = happyGoto action_84
action_65 (32) = happyGoto action_85
action_65 (33) = happyGoto action_86
action_65 (34) = happyGoto action_87
action_65 (35) = happyGoto action_88
action_65 _ = happyFail

action_66 (53) = happyShift action_38
action_66 (54) = happyShift action_89
action_66 (57) = happyShift action_90
action_66 (63) = happyShift action_91
action_66 (65) = happyShift action_92
action_66 (73) = happyShift action_93
action_66 (74) = happyShift action_94
action_66 (75) = happyShift action_95
action_66 (76) = happyShift action_96
action_66 (77) = happyShift action_97
action_66 (78) = happyShift action_98
action_66 (79) = happyShift action_99
action_66 (83) = happyShift action_100
action_66 (85) = happyShift action_101
action_66 (87) = happyShift action_102
action_66 (91) = happyShift action_52
action_66 (95) = happyShift action_53
action_66 (96) = happyShift action_54
action_66 (97) = happyShift action_55
action_66 (99) = happyShift action_56
action_66 (100) = happyShift action_57
action_66 (102) = happyShift action_103
action_66 (103) = happyShift action_104
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
action_66 (34) = happyGoto action_87
action_66 (35) = happyGoto action_88
action_66 _ = happyFail

action_67 (94) = happyShift action_76
action_67 _ = happyFail

action_68 (55) = happyShift action_14
action_68 (56) = happyShift action_15
action_68 (57) = happyShift action_16
action_68 (58) = happyShift action_17
action_68 (60) = happyShift action_18
action_68 (61) = happyShift action_19
action_68 (62) = happyShift action_20
action_68 (65) = happyShift action_21
action_68 (67) = happyShift action_22
action_68 (68) = happyShift action_23
action_68 (69) = happyShift action_24
action_68 (70) = happyShift action_25
action_68 (71) = happyShift action_26
action_68 (82) = happyShift action_75
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

action_69 (55) = happyShift action_14
action_69 (56) = happyShift action_15
action_69 (57) = happyShift action_16
action_69 (58) = happyShift action_17
action_69 (60) = happyShift action_18
action_69 (61) = happyShift action_19
action_69 (62) = happyShift action_20
action_69 (65) = happyShift action_21
action_69 (67) = happyShift action_22
action_69 (68) = happyShift action_23
action_69 (69) = happyShift action_24
action_69 (70) = happyShift action_25
action_69 (71) = happyShift action_26
action_69 (82) = happyShift action_74
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

action_70 (55) = happyShift action_14
action_70 (56) = happyShift action_15
action_70 (57) = happyShift action_16
action_70 (58) = happyShift action_17
action_70 (60) = happyShift action_18
action_70 (61) = happyShift action_19
action_70 (62) = happyShift action_20
action_70 (65) = happyShift action_21
action_70 (67) = happyShift action_22
action_70 (68) = happyShift action_23
action_70 (69) = happyShift action_24
action_70 (70) = happyShift action_25
action_70 (71) = happyShift action_26
action_70 (82) = happyShift action_73
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

action_71 (55) = happyShift action_14
action_71 (56) = happyShift action_15
action_71 (57) = happyShift action_16
action_71 (58) = happyShift action_17
action_71 (60) = happyShift action_18
action_71 (61) = happyShift action_19
action_71 (62) = happyShift action_20
action_71 (65) = happyShift action_21
action_71 (67) = happyShift action_22
action_71 (68) = happyShift action_23
action_71 (69) = happyShift action_24
action_71 (70) = happyShift action_25
action_71 (71) = happyShift action_26
action_71 (82) = happyShift action_72
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

action_76 (53) = happyShift action_38
action_76 (54) = happyShift action_89
action_76 (57) = happyShift action_90
action_76 (63) = happyShift action_91
action_76 (65) = happyShift action_92
action_76 (73) = happyShift action_93
action_76 (74) = happyShift action_94
action_76 (75) = happyShift action_95
action_76 (76) = happyShift action_96
action_76 (77) = happyShift action_97
action_76 (78) = happyShift action_98
action_76 (79) = happyShift action_99
action_76 (83) = happyShift action_100
action_76 (85) = happyShift action_101
action_76 (87) = happyShift action_102
action_76 (91) = happyShift action_52
action_76 (95) = happyShift action_53
action_76 (96) = happyShift action_54
action_76 (97) = happyShift action_55
action_76 (99) = happyShift action_56
action_76 (100) = happyShift action_57
action_76 (102) = happyShift action_103
action_76 (103) = happyShift action_104
action_76 (17) = happyGoto action_77
action_76 (20) = happyGoto action_78
action_76 (21) = happyGoto action_79
action_76 (23) = happyGoto action_80
action_76 (25) = happyGoto action_81
action_76 (27) = happyGoto action_82
action_76 (28) = happyGoto action_184
action_76 (29) = happyGoto action_106
action_76 (30) = happyGoto action_107
action_76 (31) = happyGoto action_84
action_76 (32) = happyGoto action_85
action_76 (33) = happyGoto action_86
action_76 (34) = happyGoto action_87
action_76 (35) = happyGoto action_88
action_76 _ = happyFail

action_77 _ = happyReduce_96

action_78 (53) = happyShift action_38
action_78 (54) = happyShift action_89
action_78 (57) = happyShift action_90
action_78 (73) = happyShift action_93
action_78 (74) = happyShift action_94
action_78 (75) = happyShift action_95
action_78 (76) = happyShift action_96
action_78 (77) = happyShift action_97
action_78 (78) = happyShift action_98
action_78 (79) = happyShift action_99
action_78 (83) = happyShift action_182
action_78 (85) = happyShift action_183
action_78 (87) = happyShift action_102
action_78 (91) = happyShift action_52
action_78 (95) = happyShift action_53
action_78 (96) = happyShift action_54
action_78 (97) = happyShift action_55
action_78 (99) = happyShift action_56
action_78 (100) = happyShift action_57
action_78 (103) = happyShift action_104
action_78 (17) = happyGoto action_77
action_78 (20) = happyGoto action_176
action_78 (21) = happyGoto action_177
action_78 (23) = happyGoto action_178
action_78 (25) = happyGoto action_179
action_78 (31) = happyGoto action_180
action_78 (32) = happyGoto action_85
action_78 (33) = happyGoto action_181
action_78 (34) = happyGoto action_87
action_78 (35) = happyGoto action_88
action_78 _ = happyFail

action_79 (79) = happyShift action_174
action_79 (93) = happyShift action_175
action_79 (101) = happyReduce_82
action_79 _ = happyReduce_65

action_80 (101) = happyReduce_83
action_80 _ = happyReduce_66

action_81 (101) = happyReduce_81
action_81 _ = happyReduce_64

action_82 (92) = happyShift action_173
action_82 _ = happyFail

action_83 _ = happyReduce_107

action_84 (92) = happyReduce_61
action_84 _ = happyReduce_74

action_85 (101) = happyShift action_172
action_85 _ = happyFail

action_86 (96) = happyShift action_171
action_86 (101) = happyReduce_78
action_86 _ = happyReduce_76

action_87 (53) = happyShift action_38
action_87 (54) = happyShift action_156
action_87 (57) = happyShift action_90
action_87 (73) = happyShift action_169
action_87 (75) = happyShift action_95
action_87 (76) = happyShift action_96
action_87 (77) = happyShift action_97
action_87 (78) = happyShift action_98
action_87 (79) = happyShift action_99
action_87 (85) = happyShift action_157
action_87 (91) = happyShift action_170
action_87 (103) = happyShift action_104
action_87 (17) = happyGoto action_77
action_87 (35) = happyGoto action_168
action_87 _ = happyReduce_84

action_88 _ = happyReduce_89

action_89 (99) = happyShift action_167
action_89 _ = happyReduce_101

action_90 (81) = happyShift action_166
action_90 _ = happyFail

action_91 (53) = happyShift action_38
action_91 (54) = happyShift action_89
action_91 (57) = happyShift action_90
action_91 (63) = happyShift action_91
action_91 (65) = happyShift action_92
action_91 (73) = happyShift action_93
action_91 (74) = happyShift action_94
action_91 (75) = happyShift action_95
action_91 (76) = happyShift action_96
action_91 (77) = happyShift action_97
action_91 (78) = happyShift action_98
action_91 (79) = happyShift action_99
action_91 (83) = happyShift action_100
action_91 (85) = happyShift action_101
action_91 (87) = happyShift action_102
action_91 (91) = happyShift action_52
action_91 (95) = happyShift action_53
action_91 (96) = happyShift action_54
action_91 (97) = happyShift action_55
action_91 (99) = happyShift action_56
action_91 (100) = happyShift action_57
action_91 (102) = happyShift action_103
action_91 (103) = happyShift action_104
action_91 (17) = happyGoto action_77
action_91 (20) = happyGoto action_78
action_91 (21) = happyGoto action_79
action_91 (23) = happyGoto action_80
action_91 (25) = happyGoto action_81
action_91 (27) = happyGoto action_82
action_91 (28) = happyGoto action_165
action_91 (29) = happyGoto action_106
action_91 (30) = happyGoto action_107
action_91 (31) = happyGoto action_84
action_91 (32) = happyGoto action_85
action_91 (33) = happyGoto action_86
action_91 (34) = happyGoto action_87
action_91 (35) = happyGoto action_88
action_91 _ = happyFail

action_92 (53) = happyShift action_38
action_92 (83) = happyShift action_163
action_92 (85) = happyShift action_164
action_92 (91) = happyShift action_52
action_92 (95) = happyShift action_53
action_92 (96) = happyShift action_54
action_92 (97) = happyShift action_55
action_92 (99) = happyShift action_56
action_92 (100) = happyShift action_57
action_92 (17) = happyGoto action_160
action_92 (20) = happyGoto action_161
action_92 (26) = happyGoto action_162
action_92 _ = happyFail

action_93 (53) = happyShift action_38
action_93 (54) = happyShift action_156
action_93 (57) = happyShift action_90
action_93 (75) = happyShift action_95
action_93 (76) = happyShift action_96
action_93 (77) = happyShift action_97
action_93 (78) = happyShift action_98
action_93 (79) = happyShift action_99
action_93 (85) = happyShift action_157
action_93 (103) = happyShift action_104
action_93 (17) = happyGoto action_77
action_93 (35) = happyGoto action_159
action_93 _ = happyReduce_86

action_94 (53) = happyShift action_38
action_94 (54) = happyShift action_156
action_94 (57) = happyShift action_90
action_94 (75) = happyShift action_95
action_94 (76) = happyShift action_96
action_94 (77) = happyShift action_97
action_94 (78) = happyShift action_98
action_94 (79) = happyShift action_99
action_94 (85) = happyShift action_157
action_94 (103) = happyShift action_104
action_94 (17) = happyGoto action_77
action_94 (35) = happyGoto action_158
action_94 _ = happyFail

action_95 _ = happyReduce_93

action_96 _ = happyReduce_95

action_97 (53) = happyShift action_38
action_97 (54) = happyShift action_156
action_97 (57) = happyShift action_90
action_97 (75) = happyShift action_95
action_97 (76) = happyShift action_96
action_97 (77) = happyShift action_97
action_97 (78) = happyShift action_98
action_97 (79) = happyShift action_99
action_97 (85) = happyShift action_157
action_97 (103) = happyShift action_104
action_97 (17) = happyGoto action_77
action_97 (35) = happyGoto action_155
action_97 _ = happyFail

action_98 _ = happyReduce_94

action_99 (53) = happyShift action_38
action_99 (54) = happyShift action_89
action_99 (57) = happyShift action_90
action_99 (63) = happyShift action_91
action_99 (65) = happyShift action_92
action_99 (73) = happyShift action_93
action_99 (74) = happyShift action_94
action_99 (75) = happyShift action_95
action_99 (76) = happyShift action_96
action_99 (77) = happyShift action_97
action_99 (78) = happyShift action_98
action_99 (79) = happyShift action_99
action_99 (83) = happyShift action_100
action_99 (85) = happyShift action_101
action_99 (87) = happyShift action_102
action_99 (91) = happyShift action_52
action_99 (95) = happyShift action_53
action_99 (96) = happyShift action_54
action_99 (97) = happyShift action_55
action_99 (99) = happyShift action_56
action_99 (100) = happyShift action_57
action_99 (102) = happyShift action_103
action_99 (103) = happyShift action_104
action_99 (17) = happyGoto action_77
action_99 (20) = happyGoto action_78
action_99 (21) = happyGoto action_79
action_99 (23) = happyGoto action_80
action_99 (25) = happyGoto action_81
action_99 (27) = happyGoto action_82
action_99 (28) = happyGoto action_154
action_99 (29) = happyGoto action_106
action_99 (30) = happyGoto action_107
action_99 (31) = happyGoto action_84
action_99 (32) = happyGoto action_85
action_99 (33) = happyGoto action_86
action_99 (34) = happyGoto action_87
action_99 (35) = happyGoto action_88
action_99 _ = happyFail

action_100 (53) = happyShift action_38
action_100 (54) = happyShift action_89
action_100 (57) = happyShift action_90
action_100 (63) = happyShift action_91
action_100 (65) = happyShift action_92
action_100 (73) = happyShift action_93
action_100 (74) = happyShift action_94
action_100 (75) = happyShift action_95
action_100 (76) = happyShift action_96
action_100 (77) = happyShift action_97
action_100 (78) = happyShift action_98
action_100 (79) = happyShift action_99
action_100 (83) = happyShift action_100
action_100 (85) = happyShift action_101
action_100 (87) = happyShift action_102
action_100 (91) = happyShift action_52
action_100 (95) = happyShift action_53
action_100 (96) = happyShift action_54
action_100 (97) = happyShift action_55
action_100 (99) = happyShift action_56
action_100 (100) = happyShift action_57
action_100 (102) = happyShift action_103
action_100 (103) = happyShift action_104
action_100 (17) = happyGoto action_151
action_100 (19) = happyGoto action_152
action_100 (20) = happyGoto action_78
action_100 (21) = happyGoto action_79
action_100 (23) = happyGoto action_80
action_100 (25) = happyGoto action_81
action_100 (27) = happyGoto action_82
action_100 (30) = happyGoto action_153
action_100 (31) = happyGoto action_84
action_100 (32) = happyGoto action_85
action_100 (33) = happyGoto action_86
action_100 (34) = happyGoto action_87
action_100 (35) = happyGoto action_88
action_100 _ = happyFail

action_101 (53) = happyShift action_38
action_101 (54) = happyShift action_89
action_101 (57) = happyShift action_90
action_101 (63) = happyShift action_91
action_101 (65) = happyShift action_92
action_101 (73) = happyShift action_93
action_101 (74) = happyShift action_94
action_101 (75) = happyShift action_95
action_101 (76) = happyShift action_96
action_101 (77) = happyShift action_97
action_101 (78) = happyShift action_98
action_101 (79) = happyShift action_99
action_101 (83) = happyShift action_100
action_101 (85) = happyShift action_101
action_101 (87) = happyShift action_102
action_101 (91) = happyShift action_52
action_101 (95) = happyShift action_53
action_101 (96) = happyShift action_54
action_101 (97) = happyShift action_55
action_101 (99) = happyShift action_56
action_101 (100) = happyShift action_57
action_101 (102) = happyShift action_103
action_101 (103) = happyShift action_104
action_101 (17) = happyGoto action_147
action_101 (20) = happyGoto action_78
action_101 (21) = happyGoto action_79
action_101 (23) = happyGoto action_80
action_101 (24) = happyGoto action_148
action_101 (25) = happyGoto action_81
action_101 (27) = happyGoto action_82
action_101 (28) = happyGoto action_149
action_101 (29) = happyGoto action_150
action_101 (30) = happyGoto action_107
action_101 (31) = happyGoto action_84
action_101 (32) = happyGoto action_85
action_101 (33) = happyGoto action_86
action_101 (34) = happyGoto action_87
action_101 (35) = happyGoto action_88
action_101 _ = happyFail

action_102 (53) = happyShift action_38
action_102 (54) = happyShift action_89
action_102 (57) = happyShift action_90
action_102 (63) = happyShift action_91
action_102 (65) = happyShift action_92
action_102 (73) = happyShift action_93
action_102 (74) = happyShift action_94
action_102 (75) = happyShift action_95
action_102 (76) = happyShift action_96
action_102 (77) = happyShift action_97
action_102 (78) = happyShift action_98
action_102 (79) = happyShift action_99
action_102 (83) = happyShift action_100
action_102 (85) = happyShift action_101
action_102 (87) = happyShift action_102
action_102 (91) = happyShift action_52
action_102 (95) = happyShift action_53
action_102 (96) = happyShift action_54
action_102 (97) = happyShift action_55
action_102 (99) = happyShift action_56
action_102 (100) = happyShift action_57
action_102 (102) = happyShift action_103
action_102 (103) = happyShift action_104
action_102 (17) = happyGoto action_77
action_102 (20) = happyGoto action_78
action_102 (21) = happyGoto action_79
action_102 (22) = happyGoto action_145
action_102 (23) = happyGoto action_80
action_102 (25) = happyGoto action_81
action_102 (27) = happyGoto action_82
action_102 (30) = happyGoto action_146
action_102 (31) = happyGoto action_84
action_102 (32) = happyGoto action_85
action_102 (33) = happyGoto action_86
action_102 (34) = happyGoto action_87
action_102 (35) = happyGoto action_88
action_102 _ = happyFail

action_103 (53) = happyShift action_38
action_103 (17) = happyGoto action_143
action_103 (18) = happyGoto action_144
action_103 _ = happyFail

action_104 _ = happyReduce_99

action_105 _ = happyReduce_29

action_106 _ = happyReduce_67

action_107 (88) = happyShift action_142
action_107 _ = happyReduce_68

action_108 (43) = happyGoto action_140
action_108 (44) = happyGoto action_141
action_108 _ = happyReduce_121

action_109 _ = happyReduce_140

action_110 (82) = happyShift action_139
action_110 _ = happyFail

action_111 (89) = happyShift action_138
action_111 _ = happyReduce_137

action_112 (82) = happyShift action_137
action_112 _ = happyFail

action_113 _ = happyReduce_28

action_114 (90) = happyShift action_136
action_114 _ = happyFail

action_115 (90) = happyShift action_135
action_115 _ = happyFail

action_116 (53) = happyShift action_38
action_116 (54) = happyShift action_89
action_116 (57) = happyShift action_90
action_116 (63) = happyShift action_91
action_116 (65) = happyShift action_92
action_116 (73) = happyShift action_93
action_116 (74) = happyShift action_94
action_116 (75) = happyShift action_95
action_116 (76) = happyShift action_96
action_116 (77) = happyShift action_97
action_116 (78) = happyShift action_98
action_116 (79) = happyShift action_99
action_116 (83) = happyShift action_100
action_116 (85) = happyShift action_101
action_116 (87) = happyShift action_102
action_116 (91) = happyShift action_52
action_116 (95) = happyShift action_53
action_116 (96) = happyShift action_54
action_116 (97) = happyShift action_55
action_116 (99) = happyShift action_56
action_116 (100) = happyShift action_57
action_116 (102) = happyShift action_103
action_116 (103) = happyShift action_104
action_116 (17) = happyGoto action_77
action_116 (20) = happyGoto action_78
action_116 (21) = happyGoto action_79
action_116 (23) = happyGoto action_80
action_116 (25) = happyGoto action_81
action_116 (27) = happyGoto action_82
action_116 (30) = happyGoto action_134
action_116 (31) = happyGoto action_84
action_116 (32) = happyGoto action_85
action_116 (33) = happyGoto action_86
action_116 (34) = happyGoto action_87
action_116 (35) = happyGoto action_88
action_116 _ = happyFail

action_117 (53) = happyShift action_38
action_117 (54) = happyShift action_89
action_117 (57) = happyShift action_90
action_117 (63) = happyShift action_91
action_117 (65) = happyShift action_92
action_117 (73) = happyShift action_93
action_117 (74) = happyShift action_94
action_117 (75) = happyShift action_95
action_117 (76) = happyShift action_96
action_117 (77) = happyShift action_97
action_117 (78) = happyShift action_98
action_117 (79) = happyShift action_99
action_117 (83) = happyShift action_100
action_117 (85) = happyShift action_101
action_117 (87) = happyShift action_102
action_117 (91) = happyShift action_52
action_117 (95) = happyShift action_53
action_117 (96) = happyShift action_54
action_117 (97) = happyShift action_55
action_117 (99) = happyShift action_56
action_117 (100) = happyShift action_57
action_117 (102) = happyShift action_103
action_117 (103) = happyShift action_104
action_117 (17) = happyGoto action_77
action_117 (20) = happyGoto action_78
action_117 (21) = happyGoto action_79
action_117 (23) = happyGoto action_80
action_117 (25) = happyGoto action_81
action_117 (27) = happyGoto action_82
action_117 (30) = happyGoto action_133
action_117 (31) = happyGoto action_84
action_117 (32) = happyGoto action_85
action_117 (33) = happyGoto action_86
action_117 (34) = happyGoto action_87
action_117 (35) = happyGoto action_88
action_117 _ = happyFail

action_118 (88) = happyShift action_132
action_118 _ = happyReduce_36

action_119 (90) = happyShift action_131
action_119 _ = happyFail

action_120 (53) = happyShift action_38
action_120 (17) = happyGoto action_130
action_120 _ = happyFail

action_121 (53) = happyShift action_38
action_121 (17) = happyGoto action_118
action_121 (19) = happyGoto action_129
action_121 _ = happyFail

action_122 (90) = happyShift action_128
action_122 _ = happyFail

action_123 (53) = happyShift action_38
action_123 (54) = happyShift action_89
action_123 (57) = happyShift action_90
action_123 (63) = happyShift action_91
action_123 (65) = happyShift action_92
action_123 (73) = happyShift action_93
action_123 (74) = happyShift action_94
action_123 (75) = happyShift action_95
action_123 (76) = happyShift action_96
action_123 (77) = happyShift action_97
action_123 (78) = happyShift action_98
action_123 (79) = happyShift action_99
action_123 (83) = happyShift action_100
action_123 (85) = happyShift action_101
action_123 (87) = happyShift action_102
action_123 (91) = happyShift action_52
action_123 (95) = happyShift action_53
action_123 (96) = happyShift action_54
action_123 (97) = happyShift action_55
action_123 (99) = happyShift action_56
action_123 (100) = happyShift action_57
action_123 (102) = happyShift action_103
action_123 (103) = happyShift action_104
action_123 (17) = happyGoto action_77
action_123 (20) = happyGoto action_78
action_123 (21) = happyGoto action_79
action_123 (23) = happyGoto action_80
action_123 (25) = happyGoto action_81
action_123 (27) = happyGoto action_82
action_123 (30) = happyGoto action_127
action_123 (31) = happyGoto action_84
action_123 (32) = happyGoto action_85
action_123 (33) = happyGoto action_86
action_123 (34) = happyGoto action_87
action_123 (35) = happyGoto action_88
action_123 _ = happyFail

action_124 _ = happyReduce_148

action_125 (53) = happyShift action_38
action_125 (17) = happyGoto action_118
action_125 (19) = happyGoto action_126
action_125 _ = happyFail

action_126 (90) = happyShift action_237
action_126 _ = happyFail

action_127 (81) = happyShift action_236
action_127 _ = happyFail

action_128 (53) = happyShift action_38
action_128 (54) = happyShift action_89
action_128 (57) = happyShift action_90
action_128 (63) = happyShift action_91
action_128 (65) = happyShift action_92
action_128 (73) = happyShift action_93
action_128 (74) = happyShift action_94
action_128 (75) = happyShift action_95
action_128 (76) = happyShift action_96
action_128 (77) = happyShift action_97
action_128 (78) = happyShift action_98
action_128 (79) = happyShift action_99
action_128 (83) = happyShift action_100
action_128 (85) = happyShift action_101
action_128 (87) = happyShift action_102
action_128 (91) = happyShift action_52
action_128 (95) = happyShift action_53
action_128 (96) = happyShift action_54
action_128 (97) = happyShift action_55
action_128 (99) = happyShift action_56
action_128 (100) = happyShift action_57
action_128 (102) = happyShift action_103
action_128 (103) = happyShift action_104
action_128 (17) = happyGoto action_77
action_128 (20) = happyGoto action_78
action_128 (21) = happyGoto action_79
action_128 (23) = happyGoto action_80
action_128 (25) = happyGoto action_81
action_128 (27) = happyGoto action_82
action_128 (30) = happyGoto action_235
action_128 (31) = happyGoto action_84
action_128 (32) = happyGoto action_85
action_128 (33) = happyGoto action_86
action_128 (34) = happyGoto action_87
action_128 (35) = happyGoto action_88
action_128 _ = happyFail

action_129 (90) = happyShift action_234
action_129 _ = happyFail

action_130 (86) = happyShift action_233
action_130 _ = happyFail

action_131 (53) = happyShift action_38
action_131 (54) = happyShift action_89
action_131 (57) = happyShift action_90
action_131 (63) = happyShift action_91
action_131 (65) = happyShift action_92
action_131 (73) = happyShift action_93
action_131 (74) = happyShift action_94
action_131 (75) = happyShift action_95
action_131 (76) = happyShift action_96
action_131 (77) = happyShift action_97
action_131 (78) = happyShift action_98
action_131 (79) = happyShift action_99
action_131 (83) = happyShift action_100
action_131 (85) = happyShift action_101
action_131 (87) = happyShift action_102
action_131 (91) = happyShift action_52
action_131 (95) = happyShift action_53
action_131 (96) = happyShift action_54
action_131 (97) = happyShift action_55
action_131 (99) = happyShift action_56
action_131 (100) = happyShift action_57
action_131 (102) = happyShift action_103
action_131 (103) = happyShift action_104
action_131 (17) = happyGoto action_77
action_131 (20) = happyGoto action_78
action_131 (21) = happyGoto action_79
action_131 (23) = happyGoto action_80
action_131 (25) = happyGoto action_81
action_131 (27) = happyGoto action_82
action_131 (30) = happyGoto action_232
action_131 (31) = happyGoto action_84
action_131 (32) = happyGoto action_85
action_131 (33) = happyGoto action_86
action_131 (34) = happyGoto action_87
action_131 (35) = happyGoto action_88
action_131 _ = happyFail

action_132 (53) = happyShift action_38
action_132 (17) = happyGoto action_118
action_132 (19) = happyGoto action_231
action_132 _ = happyFail

action_133 (81) = happyShift action_230
action_133 _ = happyFail

action_134 (81) = happyShift action_229
action_134 _ = happyFail

action_135 (53) = happyShift action_38
action_135 (54) = happyShift action_89
action_135 (57) = happyShift action_90
action_135 (63) = happyShift action_91
action_135 (65) = happyShift action_92
action_135 (73) = happyShift action_93
action_135 (74) = happyShift action_94
action_135 (75) = happyShift action_95
action_135 (76) = happyShift action_96
action_135 (77) = happyShift action_97
action_135 (78) = happyShift action_98
action_135 (79) = happyShift action_99
action_135 (83) = happyShift action_100
action_135 (85) = happyShift action_101
action_135 (87) = happyShift action_102
action_135 (91) = happyShift action_52
action_135 (95) = happyShift action_53
action_135 (96) = happyShift action_54
action_135 (97) = happyShift action_55
action_135 (99) = happyShift action_56
action_135 (100) = happyShift action_57
action_135 (102) = happyShift action_103
action_135 (103) = happyShift action_104
action_135 (17) = happyGoto action_77
action_135 (20) = happyGoto action_78
action_135 (21) = happyGoto action_79
action_135 (23) = happyGoto action_80
action_135 (25) = happyGoto action_81
action_135 (27) = happyGoto action_82
action_135 (30) = happyGoto action_228
action_135 (31) = happyGoto action_84
action_135 (32) = happyGoto action_85
action_135 (33) = happyGoto action_86
action_135 (34) = happyGoto action_87
action_135 (35) = happyGoto action_88
action_135 _ = happyFail

action_136 (53) = happyShift action_38
action_136 (54) = happyShift action_89
action_136 (57) = happyShift action_90
action_136 (63) = happyShift action_91
action_136 (65) = happyShift action_92
action_136 (73) = happyShift action_93
action_136 (74) = happyShift action_94
action_136 (75) = happyShift action_95
action_136 (76) = happyShift action_96
action_136 (77) = happyShift action_97
action_136 (78) = happyShift action_98
action_136 (79) = happyShift action_99
action_136 (83) = happyShift action_100
action_136 (85) = happyShift action_101
action_136 (87) = happyShift action_102
action_136 (91) = happyShift action_52
action_136 (95) = happyShift action_53
action_136 (96) = happyShift action_54
action_136 (97) = happyShift action_55
action_136 (99) = happyShift action_56
action_136 (100) = happyShift action_57
action_136 (102) = happyShift action_103
action_136 (103) = happyShift action_104
action_136 (17) = happyGoto action_77
action_136 (20) = happyGoto action_78
action_136 (21) = happyGoto action_79
action_136 (23) = happyGoto action_80
action_136 (25) = happyGoto action_81
action_136 (27) = happyGoto action_82
action_136 (30) = happyGoto action_227
action_136 (31) = happyGoto action_84
action_136 (32) = happyGoto action_85
action_136 (33) = happyGoto action_86
action_136 (34) = happyGoto action_87
action_136 (35) = happyGoto action_88
action_136 _ = happyFail

action_137 _ = happyReduce_26

action_138 (53) = happyShift action_38
action_138 (17) = happyGoto action_108
action_138 (42) = happyGoto action_226
action_138 _ = happyReduce_139

action_139 _ = happyReduce_27

action_140 (94) = happyShift action_225
action_140 _ = happyReduce_119

action_141 (53) = happyShift action_38
action_141 (77) = happyShift action_222
action_141 (85) = happyShift action_223
action_141 (91) = happyShift action_224
action_141 (17) = happyGoto action_220
action_141 (45) = happyGoto action_221
action_141 _ = happyReduce_120

action_142 (53) = happyShift action_38
action_142 (54) = happyShift action_89
action_142 (57) = happyShift action_90
action_142 (63) = happyShift action_91
action_142 (65) = happyShift action_92
action_142 (73) = happyShift action_93
action_142 (74) = happyShift action_94
action_142 (75) = happyShift action_95
action_142 (76) = happyShift action_96
action_142 (77) = happyShift action_97
action_142 (78) = happyShift action_98
action_142 (79) = happyShift action_99
action_142 (83) = happyShift action_100
action_142 (85) = happyShift action_101
action_142 (87) = happyShift action_102
action_142 (91) = happyShift action_52
action_142 (95) = happyShift action_53
action_142 (96) = happyShift action_54
action_142 (97) = happyShift action_55
action_142 (99) = happyShift action_56
action_142 (100) = happyShift action_57
action_142 (102) = happyShift action_103
action_142 (103) = happyShift action_104
action_142 (17) = happyGoto action_77
action_142 (20) = happyGoto action_78
action_142 (21) = happyGoto action_79
action_142 (23) = happyGoto action_80
action_142 (25) = happyGoto action_81
action_142 (27) = happyGoto action_82
action_142 (29) = happyGoto action_219
action_142 (30) = happyGoto action_107
action_142 (31) = happyGoto action_84
action_142 (32) = happyGoto action_85
action_142 (33) = happyGoto action_86
action_142 (34) = happyGoto action_87
action_142 (35) = happyGoto action_88
action_142 _ = happyFail

action_143 (53) = happyShift action_38
action_143 (17) = happyGoto action_143
action_143 (18) = happyGoto action_218
action_143 _ = happyReduce_34

action_144 (92) = happyShift action_217
action_144 _ = happyFail

action_145 _ = happyReduce_44

action_146 (87) = happyShift action_215
action_146 (88) = happyShift action_216
action_146 _ = happyFail

action_147 (79) = happyShift action_213
action_147 (93) = happyShift action_214
action_147 _ = happyReduce_96

action_148 (90) = happyShift action_212
action_148 _ = happyFail

action_149 (86) = happyShift action_211
action_149 _ = happyFail

action_150 (90) = happyReduce_49
action_150 _ = happyReduce_67

action_151 (79) = happyShift action_209
action_151 (88) = happyShift action_132
action_151 (90) = happyReduce_36
action_151 (93) = happyShift action_210
action_151 _ = happyReduce_96

action_152 (90) = happyShift action_208
action_152 _ = happyFail

action_153 (84) = happyShift action_207
action_153 _ = happyFail

action_154 (90) = happyShift action_206
action_154 _ = happyFail

action_155 _ = happyReduce_100

action_156 _ = happyReduce_101

action_157 (53) = happyShift action_38
action_157 (54) = happyShift action_89
action_157 (57) = happyShift action_90
action_157 (63) = happyShift action_91
action_157 (65) = happyShift action_92
action_157 (73) = happyShift action_93
action_157 (74) = happyShift action_94
action_157 (75) = happyShift action_95
action_157 (76) = happyShift action_96
action_157 (77) = happyShift action_97
action_157 (78) = happyShift action_98
action_157 (79) = happyShift action_99
action_157 (83) = happyShift action_100
action_157 (85) = happyShift action_101
action_157 (87) = happyShift action_102
action_157 (91) = happyShift action_52
action_157 (95) = happyShift action_53
action_157 (96) = happyShift action_54
action_157 (97) = happyShift action_55
action_157 (99) = happyShift action_56
action_157 (100) = happyShift action_57
action_157 (102) = happyShift action_103
action_157 (103) = happyShift action_104
action_157 (17) = happyGoto action_77
action_157 (20) = happyGoto action_78
action_157 (21) = happyGoto action_79
action_157 (23) = happyGoto action_80
action_157 (25) = happyGoto action_81
action_157 (27) = happyGoto action_82
action_157 (28) = happyGoto action_149
action_157 (29) = happyGoto action_106
action_157 (30) = happyGoto action_107
action_157 (31) = happyGoto action_84
action_157 (32) = happyGoto action_85
action_157 (33) = happyGoto action_86
action_157 (34) = happyGoto action_87
action_157 (35) = happyGoto action_88
action_157 _ = happyFail

action_158 _ = happyReduce_85

action_159 _ = happyReduce_87

action_160 (90) = happyShift action_205
action_160 _ = happyFail

action_161 (85) = happyShift action_204
action_161 _ = happyFail

action_162 (94) = happyShift action_203
action_162 _ = happyFail

action_163 (53) = happyShift action_38
action_163 (17) = happyGoto action_202
action_163 _ = happyFail

action_164 (53) = happyShift action_38
action_164 (17) = happyGoto action_201
action_164 _ = happyFail

action_165 (81) = happyShift action_200
action_165 _ = happyFail

action_166 (53) = happyShift action_38
action_166 (17) = happyGoto action_143
action_166 (18) = happyGoto action_197
action_166 (36) = happyGoto action_198
action_166 (37) = happyGoto action_199
action_166 _ = happyReduce_105

action_167 (53) = happyShift action_38
action_167 (54) = happyShift action_89
action_167 (57) = happyShift action_90
action_167 (73) = happyShift action_93
action_167 (74) = happyShift action_94
action_167 (75) = happyShift action_95
action_167 (76) = happyShift action_96
action_167 (77) = happyShift action_97
action_167 (78) = happyShift action_98
action_167 (79) = happyShift action_99
action_167 (85) = happyShift action_157
action_167 (103) = happyShift action_104
action_167 (17) = happyGoto action_77
action_167 (33) = happyGoto action_196
action_167 (34) = happyGoto action_87
action_167 (35) = happyGoto action_88
action_167 _ = happyFail

action_168 _ = happyReduce_90

action_169 _ = happyReduce_92

action_170 (53) = happyShift action_38
action_170 (17) = happyGoto action_195
action_170 _ = happyFail

action_171 (53) = happyShift action_38
action_171 (54) = happyShift action_89
action_171 (57) = happyShift action_90
action_171 (63) = happyShift action_91
action_171 (65) = happyShift action_92
action_171 (73) = happyShift action_93
action_171 (74) = happyShift action_94
action_171 (75) = happyShift action_95
action_171 (76) = happyShift action_96
action_171 (77) = happyShift action_97
action_171 (78) = happyShift action_98
action_171 (79) = happyShift action_99
action_171 (83) = happyShift action_100
action_171 (85) = happyShift action_101
action_171 (87) = happyShift action_102
action_171 (91) = happyShift action_52
action_171 (95) = happyShift action_53
action_171 (96) = happyShift action_54
action_171 (97) = happyShift action_55
action_171 (99) = happyShift action_56
action_171 (100) = happyShift action_57
action_171 (102) = happyShift action_103
action_171 (103) = happyShift action_104
action_171 (17) = happyGoto action_77
action_171 (20) = happyGoto action_78
action_171 (21) = happyGoto action_79
action_171 (23) = happyGoto action_80
action_171 (25) = happyGoto action_81
action_171 (27) = happyGoto action_82
action_171 (30) = happyGoto action_194
action_171 (31) = happyGoto action_84
action_171 (32) = happyGoto action_85
action_171 (33) = happyGoto action_86
action_171 (34) = happyGoto action_87
action_171 (35) = happyGoto action_88
action_171 _ = happyFail

action_172 (53) = happyShift action_38
action_172 (54) = happyShift action_89
action_172 (57) = happyShift action_90
action_172 (73) = happyShift action_93
action_172 (74) = happyShift action_94
action_172 (75) = happyShift action_95
action_172 (76) = happyShift action_96
action_172 (77) = happyShift action_97
action_172 (78) = happyShift action_98
action_172 (79) = happyShift action_99
action_172 (83) = happyShift action_182
action_172 (85) = happyShift action_101
action_172 (87) = happyShift action_102
action_172 (91) = happyShift action_52
action_172 (95) = happyShift action_53
action_172 (96) = happyShift action_54
action_172 (97) = happyShift action_55
action_172 (99) = happyShift action_56
action_172 (100) = happyShift action_57
action_172 (103) = happyShift action_104
action_172 (17) = happyGoto action_77
action_172 (20) = happyGoto action_176
action_172 (21) = happyGoto action_177
action_172 (23) = happyGoto action_178
action_172 (25) = happyGoto action_179
action_172 (31) = happyGoto action_192
action_172 (32) = happyGoto action_85
action_172 (33) = happyGoto action_193
action_172 (34) = happyGoto action_87
action_172 (35) = happyGoto action_88
action_172 _ = happyFail

action_173 (53) = happyShift action_38
action_173 (54) = happyShift action_89
action_173 (57) = happyShift action_90
action_173 (63) = happyShift action_91
action_173 (65) = happyShift action_92
action_173 (73) = happyShift action_93
action_173 (74) = happyShift action_94
action_173 (75) = happyShift action_95
action_173 (76) = happyShift action_96
action_173 (77) = happyShift action_97
action_173 (78) = happyShift action_98
action_173 (79) = happyShift action_99
action_173 (83) = happyShift action_100
action_173 (85) = happyShift action_101
action_173 (87) = happyShift action_102
action_173 (91) = happyShift action_52
action_173 (95) = happyShift action_53
action_173 (96) = happyShift action_54
action_173 (97) = happyShift action_55
action_173 (99) = happyShift action_56
action_173 (100) = happyShift action_57
action_173 (102) = happyShift action_103
action_173 (103) = happyShift action_104
action_173 (17) = happyGoto action_77
action_173 (20) = happyGoto action_78
action_173 (21) = happyGoto action_79
action_173 (23) = happyGoto action_80
action_173 (25) = happyGoto action_81
action_173 (27) = happyGoto action_82
action_173 (30) = happyGoto action_191
action_173 (31) = happyGoto action_84
action_173 (32) = happyGoto action_85
action_173 (33) = happyGoto action_86
action_173 (34) = happyGoto action_87
action_173 (35) = happyGoto action_88
action_173 _ = happyFail

action_174 (87) = happyShift action_102
action_174 (21) = happyGoto action_190
action_174 _ = happyFail

action_175 (87) = happyShift action_102
action_175 (21) = happyGoto action_189
action_175 _ = happyFail

action_176 (53) = happyShift action_38
action_176 (54) = happyShift action_89
action_176 (57) = happyShift action_90
action_176 (73) = happyShift action_93
action_176 (74) = happyShift action_94
action_176 (75) = happyShift action_95
action_176 (76) = happyShift action_96
action_176 (77) = happyShift action_97
action_176 (78) = happyShift action_98
action_176 (79) = happyShift action_99
action_176 (85) = happyShift action_188
action_176 (103) = happyShift action_104
action_176 (17) = happyGoto action_77
action_176 (33) = happyGoto action_187
action_176 (34) = happyGoto action_87
action_176 (35) = happyGoto action_88
action_176 _ = happyFail

action_177 (79) = happyShift action_174
action_177 (93) = happyShift action_175
action_177 _ = happyReduce_82

action_178 _ = happyReduce_83

action_179 _ = happyReduce_81

action_180 _ = happyReduce_63

action_181 (101) = happyReduce_80
action_181 _ = happyReduce_76

action_182 (53) = happyShift action_38
action_182 (54) = happyShift action_89
action_182 (57) = happyShift action_90
action_182 (63) = happyShift action_91
action_182 (65) = happyShift action_92
action_182 (73) = happyShift action_93
action_182 (74) = happyShift action_94
action_182 (75) = happyShift action_95
action_182 (76) = happyShift action_96
action_182 (77) = happyShift action_97
action_182 (78) = happyShift action_98
action_182 (79) = happyShift action_99
action_182 (83) = happyShift action_100
action_182 (85) = happyShift action_101
action_182 (87) = happyShift action_102
action_182 (91) = happyShift action_52
action_182 (95) = happyShift action_53
action_182 (96) = happyShift action_54
action_182 (97) = happyShift action_55
action_182 (99) = happyShift action_56
action_182 (100) = happyShift action_57
action_182 (102) = happyShift action_103
action_182 (103) = happyShift action_104
action_182 (17) = happyGoto action_151
action_182 (19) = happyGoto action_152
action_182 (20) = happyGoto action_78
action_182 (21) = happyGoto action_79
action_182 (23) = happyGoto action_80
action_182 (25) = happyGoto action_81
action_182 (27) = happyGoto action_82
action_182 (30) = happyGoto action_186
action_182 (31) = happyGoto action_84
action_182 (32) = happyGoto action_85
action_182 (33) = happyGoto action_86
action_182 (34) = happyGoto action_87
action_182 (35) = happyGoto action_88
action_182 _ = happyFail

action_183 (53) = happyShift action_38
action_183 (54) = happyShift action_89
action_183 (57) = happyShift action_90
action_183 (63) = happyShift action_91
action_183 (65) = happyShift action_92
action_183 (73) = happyShift action_93
action_183 (74) = happyShift action_94
action_183 (75) = happyShift action_95
action_183 (76) = happyShift action_96
action_183 (77) = happyShift action_97
action_183 (78) = happyShift action_98
action_183 (79) = happyShift action_99
action_183 (83) = happyShift action_100
action_183 (85) = happyShift action_101
action_183 (87) = happyShift action_102
action_183 (91) = happyShift action_52
action_183 (95) = happyShift action_53
action_183 (96) = happyShift action_54
action_183 (97) = happyShift action_55
action_183 (99) = happyShift action_56
action_183 (100) = happyShift action_57
action_183 (102) = happyShift action_103
action_183 (103) = happyShift action_104
action_183 (17) = happyGoto action_147
action_183 (20) = happyGoto action_78
action_183 (21) = happyGoto action_79
action_183 (23) = happyGoto action_80
action_183 (24) = happyGoto action_185
action_183 (25) = happyGoto action_81
action_183 (27) = happyGoto action_82
action_183 (28) = happyGoto action_149
action_183 (29) = happyGoto action_150
action_183 (30) = happyGoto action_107
action_183 (31) = happyGoto action_84
action_183 (32) = happyGoto action_85
action_183 (33) = happyGoto action_86
action_183 (34) = happyGoto action_87
action_183 (35) = happyGoto action_88
action_183 _ = happyFail

action_184 _ = happyReduce_30

action_185 (90) = happyShift action_280
action_185 _ = happyFail

action_186 (84) = happyShift action_279
action_186 _ = happyFail

action_187 _ = happyReduce_80

action_188 (53) = happyShift action_38
action_188 (54) = happyShift action_89
action_188 (57) = happyShift action_90
action_188 (63) = happyShift action_91
action_188 (65) = happyShift action_92
action_188 (73) = happyShift action_93
action_188 (74) = happyShift action_94
action_188 (75) = happyShift action_95
action_188 (76) = happyShift action_96
action_188 (77) = happyShift action_97
action_188 (78) = happyShift action_98
action_188 (79) = happyShift action_99
action_188 (83) = happyShift action_100
action_188 (85) = happyShift action_101
action_188 (87) = happyShift action_102
action_188 (91) = happyShift action_52
action_188 (95) = happyShift action_53
action_188 (96) = happyShift action_54
action_188 (97) = happyShift action_55
action_188 (99) = happyShift action_56
action_188 (100) = happyShift action_57
action_188 (102) = happyShift action_103
action_188 (103) = happyShift action_104
action_188 (17) = happyGoto action_77
action_188 (20) = happyGoto action_78
action_188 (21) = happyGoto action_79
action_188 (23) = happyGoto action_80
action_188 (24) = happyGoto action_278
action_188 (25) = happyGoto action_81
action_188 (27) = happyGoto action_82
action_188 (28) = happyGoto action_149
action_188 (29) = happyGoto action_150
action_188 (30) = happyGoto action_107
action_188 (31) = happyGoto action_84
action_188 (32) = happyGoto action_85
action_188 (33) = happyGoto action_86
action_188 (34) = happyGoto action_87
action_188 (35) = happyGoto action_88
action_188 _ = happyFail

action_189 _ = happyReduce_48

action_190 _ = happyReduce_47

action_191 _ = happyReduce_70

action_192 _ = happyReduce_77

action_193 (101) = happyReduce_78
action_193 _ = happyReduce_76

action_194 _ = happyReduce_75

action_195 _ = happyReduce_91

action_196 _ = happyReduce_88

action_197 (94) = happyShift action_277
action_197 _ = happyFail

action_198 (82) = happyShift action_276
action_198 _ = happyFail

action_199 (89) = happyShift action_275
action_199 _ = happyReduce_104

action_200 (53) = happyShift action_38
action_200 (77) = happyShift action_222
action_200 (85) = happyShift action_223
action_200 (91) = happyShift action_224
action_200 (17) = happyGoto action_220
action_200 (41) = happyGoto action_273
action_200 (45) = happyGoto action_274
action_200 _ = happyReduce_117

action_201 (90) = happyShift action_272
action_201 _ = happyFail

action_202 (90) = happyShift action_271
action_202 _ = happyFail

action_203 (53) = happyShift action_38
action_203 (54) = happyShift action_89
action_203 (57) = happyShift action_90
action_203 (63) = happyShift action_91
action_203 (65) = happyShift action_92
action_203 (73) = happyShift action_93
action_203 (74) = happyShift action_94
action_203 (75) = happyShift action_95
action_203 (76) = happyShift action_96
action_203 (77) = happyShift action_97
action_203 (78) = happyShift action_98
action_203 (79) = happyShift action_99
action_203 (83) = happyShift action_100
action_203 (85) = happyShift action_101
action_203 (87) = happyShift action_102
action_203 (91) = happyShift action_52
action_203 (95) = happyShift action_53
action_203 (96) = happyShift action_54
action_203 (97) = happyShift action_55
action_203 (99) = happyShift action_56
action_203 (100) = happyShift action_57
action_203 (102) = happyShift action_103
action_203 (103) = happyShift action_104
action_203 (17) = happyGoto action_77
action_203 (20) = happyGoto action_78
action_203 (21) = happyGoto action_79
action_203 (23) = happyGoto action_80
action_203 (25) = happyGoto action_81
action_203 (27) = happyGoto action_82
action_203 (28) = happyGoto action_270
action_203 (29) = happyGoto action_106
action_203 (30) = happyGoto action_107
action_203 (31) = happyGoto action_84
action_203 (32) = happyGoto action_85
action_203 (33) = happyGoto action_86
action_203 (34) = happyGoto action_87
action_203 (35) = happyGoto action_88
action_203 _ = happyFail

action_204 (53) = happyShift action_38
action_204 (17) = happyGoto action_269
action_204 _ = happyFail

action_205 (53) = happyShift action_38
action_205 (54) = happyShift action_89
action_205 (57) = happyShift action_90
action_205 (63) = happyShift action_91
action_205 (65) = happyShift action_92
action_205 (73) = happyShift action_93
action_205 (74) = happyShift action_94
action_205 (75) = happyShift action_95
action_205 (76) = happyShift action_96
action_205 (77) = happyShift action_97
action_205 (78) = happyShift action_98
action_205 (79) = happyShift action_99
action_205 (83) = happyShift action_100
action_205 (85) = happyShift action_101
action_205 (87) = happyShift action_102
action_205 (91) = happyShift action_52
action_205 (95) = happyShift action_53
action_205 (96) = happyShift action_54
action_205 (97) = happyShift action_55
action_205 (99) = happyShift action_56
action_205 (100) = happyShift action_57
action_205 (102) = happyShift action_103
action_205 (103) = happyShift action_104
action_205 (17) = happyGoto action_77
action_205 (20) = happyGoto action_78
action_205 (21) = happyGoto action_79
action_205 (23) = happyGoto action_80
action_205 (25) = happyGoto action_81
action_205 (27) = happyGoto action_82
action_205 (30) = happyGoto action_268
action_205 (31) = happyGoto action_84
action_205 (32) = happyGoto action_85
action_205 (33) = happyGoto action_86
action_205 (34) = happyGoto action_87
action_205 (35) = happyGoto action_88
action_205 _ = happyFail

action_206 (53) = happyShift action_38
action_206 (54) = happyShift action_89
action_206 (57) = happyShift action_90
action_206 (63) = happyShift action_91
action_206 (65) = happyShift action_92
action_206 (73) = happyShift action_93
action_206 (74) = happyShift action_94
action_206 (75) = happyShift action_95
action_206 (76) = happyShift action_96
action_206 (77) = happyShift action_97
action_206 (78) = happyShift action_98
action_206 (79) = happyShift action_99
action_206 (83) = happyShift action_100
action_206 (85) = happyShift action_101
action_206 (87) = happyShift action_102
action_206 (91) = happyShift action_52
action_206 (95) = happyShift action_53
action_206 (96) = happyShift action_54
action_206 (97) = happyShift action_55
action_206 (99) = happyShift action_56
action_206 (100) = happyShift action_57
action_206 (102) = happyShift action_103
action_206 (103) = happyShift action_104
action_206 (17) = happyGoto action_77
action_206 (20) = happyGoto action_78
action_206 (21) = happyGoto action_79
action_206 (23) = happyGoto action_80
action_206 (25) = happyGoto action_81
action_206 (27) = happyGoto action_82
action_206 (30) = happyGoto action_267
action_206 (31) = happyGoto action_84
action_206 (32) = happyGoto action_85
action_206 (33) = happyGoto action_86
action_206 (34) = happyGoto action_87
action_206 (35) = happyGoto action_88
action_206 _ = happyFail

action_207 (101) = happyReduce_79
action_207 _ = happyReduce_62

action_208 (53) = happyShift action_38
action_208 (54) = happyShift action_89
action_208 (57) = happyShift action_90
action_208 (63) = happyShift action_91
action_208 (65) = happyShift action_92
action_208 (73) = happyShift action_93
action_208 (74) = happyShift action_94
action_208 (75) = happyShift action_95
action_208 (76) = happyShift action_96
action_208 (77) = happyShift action_97
action_208 (78) = happyShift action_98
action_208 (79) = happyShift action_99
action_208 (83) = happyShift action_100
action_208 (85) = happyShift action_101
action_208 (87) = happyShift action_102
action_208 (91) = happyShift action_52
action_208 (95) = happyShift action_53
action_208 (96) = happyShift action_54
action_208 (97) = happyShift action_55
action_208 (99) = happyShift action_56
action_208 (100) = happyShift action_57
action_208 (102) = happyShift action_103
action_208 (103) = happyShift action_104
action_208 (17) = happyGoto action_77
action_208 (20) = happyGoto action_78
action_208 (21) = happyGoto action_79
action_208 (23) = happyGoto action_80
action_208 (25) = happyGoto action_81
action_208 (27) = happyGoto action_82
action_208 (30) = happyGoto action_266
action_208 (31) = happyGoto action_84
action_208 (32) = happyGoto action_85
action_208 (33) = happyGoto action_86
action_208 (34) = happyGoto action_87
action_208 (35) = happyGoto action_88
action_208 _ = happyFail

action_209 (53) = happyShift action_38
action_209 (54) = happyShift action_89
action_209 (57) = happyShift action_90
action_209 (63) = happyShift action_91
action_209 (65) = happyShift action_92
action_209 (73) = happyShift action_93
action_209 (74) = happyShift action_94
action_209 (75) = happyShift action_95
action_209 (76) = happyShift action_96
action_209 (77) = happyShift action_97
action_209 (78) = happyShift action_98
action_209 (79) = happyShift action_99
action_209 (83) = happyShift action_100
action_209 (85) = happyShift action_101
action_209 (87) = happyShift action_102
action_209 (91) = happyShift action_52
action_209 (95) = happyShift action_53
action_209 (96) = happyShift action_54
action_209 (97) = happyShift action_55
action_209 (99) = happyShift action_56
action_209 (100) = happyShift action_57
action_209 (102) = happyShift action_103
action_209 (103) = happyShift action_104
action_209 (17) = happyGoto action_77
action_209 (20) = happyGoto action_78
action_209 (21) = happyGoto action_79
action_209 (23) = happyGoto action_80
action_209 (25) = happyGoto action_81
action_209 (27) = happyGoto action_82
action_209 (30) = happyGoto action_265
action_209 (31) = happyGoto action_84
action_209 (32) = happyGoto action_85
action_209 (33) = happyGoto action_86
action_209 (34) = happyGoto action_87
action_209 (35) = happyGoto action_88
action_209 _ = happyFail

action_210 (53) = happyShift action_38
action_210 (54) = happyShift action_89
action_210 (57) = happyShift action_90
action_210 (63) = happyShift action_91
action_210 (65) = happyShift action_92
action_210 (73) = happyShift action_93
action_210 (74) = happyShift action_94
action_210 (75) = happyShift action_95
action_210 (76) = happyShift action_96
action_210 (77) = happyShift action_97
action_210 (78) = happyShift action_98
action_210 (79) = happyShift action_99
action_210 (83) = happyShift action_100
action_210 (85) = happyShift action_101
action_210 (87) = happyShift action_102
action_210 (91) = happyShift action_52
action_210 (95) = happyShift action_53
action_210 (96) = happyShift action_54
action_210 (97) = happyShift action_55
action_210 (99) = happyShift action_56
action_210 (100) = happyShift action_57
action_210 (102) = happyShift action_103
action_210 (103) = happyShift action_104
action_210 (17) = happyGoto action_77
action_210 (20) = happyGoto action_78
action_210 (21) = happyGoto action_79
action_210 (23) = happyGoto action_80
action_210 (25) = happyGoto action_81
action_210 (27) = happyGoto action_82
action_210 (30) = happyGoto action_264
action_210 (31) = happyGoto action_84
action_210 (32) = happyGoto action_85
action_210 (33) = happyGoto action_86
action_210 (34) = happyGoto action_87
action_210 (35) = happyGoto action_88
action_210 _ = happyFail

action_211 _ = happyReduce_98

action_212 (53) = happyShift action_38
action_212 (54) = happyShift action_89
action_212 (57) = happyShift action_90
action_212 (63) = happyShift action_91
action_212 (65) = happyShift action_92
action_212 (73) = happyShift action_93
action_212 (74) = happyShift action_94
action_212 (75) = happyShift action_95
action_212 (76) = happyShift action_96
action_212 (77) = happyShift action_97
action_212 (78) = happyShift action_98
action_212 (79) = happyShift action_99
action_212 (83) = happyShift action_100
action_212 (85) = happyShift action_101
action_212 (87) = happyShift action_102
action_212 (91) = happyShift action_52
action_212 (95) = happyShift action_53
action_212 (96) = happyShift action_54
action_212 (97) = happyShift action_55
action_212 (99) = happyShift action_56
action_212 (100) = happyShift action_57
action_212 (102) = happyShift action_103
action_212 (103) = happyShift action_104
action_212 (17) = happyGoto action_77
action_212 (20) = happyGoto action_78
action_212 (21) = happyGoto action_79
action_212 (23) = happyGoto action_80
action_212 (25) = happyGoto action_81
action_212 (27) = happyGoto action_82
action_212 (30) = happyGoto action_263
action_212 (31) = happyGoto action_84
action_212 (32) = happyGoto action_85
action_212 (33) = happyGoto action_86
action_212 (34) = happyGoto action_87
action_212 (35) = happyGoto action_88
action_212 _ = happyFail

action_213 (53) = happyShift action_38
action_213 (54) = happyShift action_89
action_213 (57) = happyShift action_90
action_213 (63) = happyShift action_91
action_213 (65) = happyShift action_92
action_213 (73) = happyShift action_93
action_213 (74) = happyShift action_94
action_213 (75) = happyShift action_95
action_213 (76) = happyShift action_96
action_213 (77) = happyShift action_97
action_213 (78) = happyShift action_98
action_213 (79) = happyShift action_99
action_213 (83) = happyShift action_100
action_213 (85) = happyShift action_101
action_213 (87) = happyShift action_102
action_213 (91) = happyShift action_52
action_213 (95) = happyShift action_53
action_213 (96) = happyShift action_54
action_213 (97) = happyShift action_55
action_213 (99) = happyShift action_56
action_213 (100) = happyShift action_57
action_213 (102) = happyShift action_103
action_213 (103) = happyShift action_104
action_213 (17) = happyGoto action_77
action_213 (20) = happyGoto action_78
action_213 (21) = happyGoto action_79
action_213 (23) = happyGoto action_80
action_213 (25) = happyGoto action_81
action_213 (27) = happyGoto action_82
action_213 (30) = happyGoto action_262
action_213 (31) = happyGoto action_84
action_213 (32) = happyGoto action_85
action_213 (33) = happyGoto action_86
action_213 (34) = happyGoto action_87
action_213 (35) = happyGoto action_88
action_213 _ = happyFail

action_214 (53) = happyShift action_38
action_214 (54) = happyShift action_89
action_214 (57) = happyShift action_90
action_214 (63) = happyShift action_91
action_214 (65) = happyShift action_92
action_214 (73) = happyShift action_93
action_214 (74) = happyShift action_94
action_214 (75) = happyShift action_95
action_214 (76) = happyShift action_96
action_214 (77) = happyShift action_97
action_214 (78) = happyShift action_98
action_214 (79) = happyShift action_99
action_214 (83) = happyShift action_100
action_214 (85) = happyShift action_101
action_214 (87) = happyShift action_102
action_214 (91) = happyShift action_52
action_214 (95) = happyShift action_53
action_214 (96) = happyShift action_54
action_214 (97) = happyShift action_55
action_214 (99) = happyShift action_56
action_214 (100) = happyShift action_57
action_214 (102) = happyShift action_103
action_214 (103) = happyShift action_104
action_214 (17) = happyGoto action_77
action_214 (20) = happyGoto action_78
action_214 (21) = happyGoto action_79
action_214 (23) = happyGoto action_80
action_214 (25) = happyGoto action_81
action_214 (27) = happyGoto action_82
action_214 (30) = happyGoto action_261
action_214 (31) = happyGoto action_84
action_214 (32) = happyGoto action_85
action_214 (33) = happyGoto action_86
action_214 (34) = happyGoto action_87
action_214 (35) = happyGoto action_88
action_214 _ = happyFail

action_215 _ = happyReduce_45

action_216 (53) = happyShift action_38
action_216 (54) = happyShift action_89
action_216 (57) = happyShift action_90
action_216 (63) = happyShift action_91
action_216 (65) = happyShift action_92
action_216 (73) = happyShift action_93
action_216 (74) = happyShift action_94
action_216 (75) = happyShift action_95
action_216 (76) = happyShift action_96
action_216 (77) = happyShift action_97
action_216 (78) = happyShift action_98
action_216 (79) = happyShift action_99
action_216 (83) = happyShift action_100
action_216 (85) = happyShift action_101
action_216 (87) = happyShift action_102
action_216 (91) = happyShift action_52
action_216 (95) = happyShift action_53
action_216 (96) = happyShift action_54
action_216 (97) = happyShift action_55
action_216 (99) = happyShift action_56
action_216 (100) = happyShift action_57
action_216 (102) = happyShift action_103
action_216 (103) = happyShift action_104
action_216 (17) = happyGoto action_77
action_216 (20) = happyGoto action_78
action_216 (21) = happyGoto action_79
action_216 (22) = happyGoto action_260
action_216 (23) = happyGoto action_80
action_216 (25) = happyGoto action_81
action_216 (27) = happyGoto action_82
action_216 (30) = happyGoto action_146
action_216 (31) = happyGoto action_84
action_216 (32) = happyGoto action_85
action_216 (33) = happyGoto action_86
action_216 (34) = happyGoto action_87
action_216 (35) = happyGoto action_88
action_216 _ = happyFail

action_217 (53) = happyShift action_38
action_217 (54) = happyShift action_89
action_217 (57) = happyShift action_90
action_217 (63) = happyShift action_91
action_217 (65) = happyShift action_92
action_217 (73) = happyShift action_93
action_217 (74) = happyShift action_94
action_217 (75) = happyShift action_95
action_217 (76) = happyShift action_96
action_217 (77) = happyShift action_97
action_217 (78) = happyShift action_98
action_217 (79) = happyShift action_99
action_217 (83) = happyShift action_100
action_217 (85) = happyShift action_101
action_217 (87) = happyShift action_102
action_217 (91) = happyShift action_52
action_217 (95) = happyShift action_53
action_217 (96) = happyShift action_54
action_217 (97) = happyShift action_55
action_217 (99) = happyShift action_56
action_217 (100) = happyShift action_57
action_217 (102) = happyShift action_103
action_217 (103) = happyShift action_104
action_217 (17) = happyGoto action_77
action_217 (20) = happyGoto action_78
action_217 (21) = happyGoto action_79
action_217 (23) = happyGoto action_80
action_217 (25) = happyGoto action_81
action_217 (27) = happyGoto action_82
action_217 (28) = happyGoto action_259
action_217 (29) = happyGoto action_106
action_217 (30) = happyGoto action_107
action_217 (31) = happyGoto action_84
action_217 (32) = happyGoto action_85
action_217 (33) = happyGoto action_86
action_217 (34) = happyGoto action_87
action_217 (35) = happyGoto action_88
action_217 _ = happyFail

action_218 _ = happyReduce_35

action_219 _ = happyReduce_69

action_220 _ = happyReduce_125

action_221 _ = happyReduce_122

action_222 (53) = happyShift action_38
action_222 (77) = happyShift action_222
action_222 (85) = happyShift action_223
action_222 (91) = happyShift action_224
action_222 (17) = happyGoto action_220
action_222 (45) = happyGoto action_258
action_222 _ = happyFail

action_223 (53) = happyShift action_38
action_223 (77) = happyShift action_222
action_223 (85) = happyShift action_223
action_223 (86) = happyShift action_257
action_223 (91) = happyShift action_224
action_223 (17) = happyGoto action_252
action_223 (45) = happyGoto action_253
action_223 (46) = happyGoto action_254
action_223 (47) = happyGoto action_255
action_223 (48) = happyGoto action_256
action_223 _ = happyFail

action_224 (53) = happyShift action_38
action_224 (54) = happyShift action_156
action_224 (57) = happyShift action_90
action_224 (73) = happyShift action_251
action_224 (75) = happyShift action_95
action_224 (76) = happyShift action_96
action_224 (77) = happyShift action_97
action_224 (78) = happyShift action_98
action_224 (79) = happyShift action_99
action_224 (85) = happyShift action_157
action_224 (103) = happyShift action_104
action_224 (17) = happyGoto action_77
action_224 (35) = happyGoto action_250
action_224 _ = happyFail

action_225 (53) = happyShift action_38
action_225 (54) = happyShift action_89
action_225 (57) = happyShift action_90
action_225 (63) = happyShift action_91
action_225 (65) = happyShift action_92
action_225 (73) = happyShift action_93
action_225 (74) = happyShift action_94
action_225 (75) = happyShift action_95
action_225 (76) = happyShift action_96
action_225 (77) = happyShift action_97
action_225 (78) = happyShift action_98
action_225 (79) = happyShift action_99
action_225 (83) = happyShift action_100
action_225 (85) = happyShift action_101
action_225 (87) = happyShift action_102
action_225 (91) = happyShift action_52
action_225 (95) = happyShift action_53
action_225 (96) = happyShift action_54
action_225 (97) = happyShift action_55
action_225 (99) = happyShift action_56
action_225 (100) = happyShift action_57
action_225 (102) = happyShift action_103
action_225 (103) = happyShift action_104
action_225 (17) = happyGoto action_77
action_225 (20) = happyGoto action_78
action_225 (21) = happyGoto action_79
action_225 (23) = happyGoto action_80
action_225 (25) = happyGoto action_81
action_225 (27) = happyGoto action_82
action_225 (28) = happyGoto action_249
action_225 (29) = happyGoto action_106
action_225 (30) = happyGoto action_107
action_225 (31) = happyGoto action_84
action_225 (32) = happyGoto action_85
action_225 (33) = happyGoto action_86
action_225 (34) = happyGoto action_87
action_225 (35) = happyGoto action_88
action_225 _ = happyFail

action_226 _ = happyReduce_138

action_227 (81) = happyShift action_248
action_227 _ = happyFail

action_228 (81) = happyShift action_247
action_228 _ = happyFail

action_229 (53) = happyShift action_38
action_229 (17) = happyGoto action_36
action_229 (38) = happyGoto action_239
action_229 (39) = happyGoto action_246
action_229 _ = happyFail

action_230 (53) = happyShift action_38
action_230 (17) = happyGoto action_36
action_230 (38) = happyGoto action_239
action_230 (39) = happyGoto action_240
action_230 (40) = happyGoto action_245
action_230 _ = happyReduce_112

action_231 _ = happyReduce_37

action_232 (86) = happyShift action_244
action_232 _ = happyFail

action_233 _ = happyReduce_146

action_234 (53) = happyShift action_38
action_234 (54) = happyShift action_89
action_234 (57) = happyShift action_90
action_234 (63) = happyShift action_91
action_234 (65) = happyShift action_92
action_234 (73) = happyShift action_93
action_234 (74) = happyShift action_94
action_234 (75) = happyShift action_95
action_234 (76) = happyShift action_96
action_234 (77) = happyShift action_97
action_234 (78) = happyShift action_98
action_234 (79) = happyShift action_99
action_234 (83) = happyShift action_100
action_234 (85) = happyShift action_101
action_234 (87) = happyShift action_102
action_234 (91) = happyShift action_52
action_234 (95) = happyShift action_53
action_234 (96) = happyShift action_54
action_234 (97) = happyShift action_55
action_234 (99) = happyShift action_56
action_234 (100) = happyShift action_57
action_234 (102) = happyShift action_103
action_234 (103) = happyShift action_104
action_234 (17) = happyGoto action_77
action_234 (20) = happyGoto action_78
action_234 (21) = happyGoto action_79
action_234 (23) = happyGoto action_80
action_234 (25) = happyGoto action_81
action_234 (27) = happyGoto action_82
action_234 (30) = happyGoto action_243
action_234 (31) = happyGoto action_84
action_234 (32) = happyGoto action_85
action_234 (33) = happyGoto action_86
action_234 (34) = happyGoto action_87
action_234 (35) = happyGoto action_88
action_234 _ = happyFail

action_235 (84) = happyShift action_242
action_235 _ = happyFail

action_236 (53) = happyShift action_38
action_236 (17) = happyGoto action_36
action_236 (38) = happyGoto action_239
action_236 (39) = happyGoto action_240
action_236 (40) = happyGoto action_241
action_236 _ = happyReduce_112

action_237 (53) = happyShift action_38
action_237 (54) = happyShift action_89
action_237 (57) = happyShift action_90
action_237 (63) = happyShift action_91
action_237 (65) = happyShift action_92
action_237 (73) = happyShift action_93
action_237 (74) = happyShift action_94
action_237 (75) = happyShift action_95
action_237 (76) = happyShift action_96
action_237 (77) = happyShift action_97
action_237 (78) = happyShift action_98
action_237 (79) = happyShift action_99
action_237 (83) = happyShift action_100
action_237 (85) = happyShift action_101
action_237 (87) = happyShift action_102
action_237 (91) = happyShift action_52
action_237 (95) = happyShift action_53
action_237 (96) = happyShift action_54
action_237 (97) = happyShift action_55
action_237 (99) = happyShift action_56
action_237 (100) = happyShift action_57
action_237 (102) = happyShift action_103
action_237 (103) = happyShift action_104
action_237 (17) = happyGoto action_77
action_237 (20) = happyGoto action_78
action_237 (21) = happyGoto action_79
action_237 (23) = happyGoto action_80
action_237 (25) = happyGoto action_81
action_237 (27) = happyGoto action_82
action_237 (30) = happyGoto action_238
action_237 (31) = happyGoto action_84
action_237 (32) = happyGoto action_85
action_237 (33) = happyGoto action_86
action_237 (34) = happyGoto action_87
action_237 (35) = happyGoto action_88
action_237 _ = happyFail

action_238 (86) = happyShift action_312
action_238 _ = happyFail

action_239 _ = happyReduce_108

action_240 _ = happyReduce_111

action_241 (82) = happyShift action_311
action_241 (89) = happyShift action_309
action_241 _ = happyFail

action_242 _ = happyReduce_143

action_243 (86) = happyShift action_310
action_243 _ = happyFail

action_244 _ = happyReduce_142

action_245 (82) = happyShift action_308
action_245 (89) = happyShift action_309
action_245 _ = happyFail

action_246 (82) = happyShift action_307
action_246 _ = happyFail

action_247 (53) = happyShift action_38
action_247 (17) = happyGoto action_36
action_247 (38) = happyGoto action_239
action_247 (39) = happyGoto action_240
action_247 (40) = happyGoto action_306
action_247 _ = happyReduce_112

action_248 (53) = happyShift action_38
action_248 (17) = happyGoto action_36
action_248 (38) = happyGoto action_239
action_248 (39) = happyGoto action_240
action_248 (40) = happyGoto action_305
action_248 _ = happyReduce_112

action_249 _ = happyReduce_118

action_250 _ = happyReduce_128

action_251 _ = happyReduce_127

action_252 (53) = happyShift action_38
action_252 (77) = happyShift action_222
action_252 (79) = happyShift action_303
action_252 (80) = happyShift action_304
action_252 (85) = happyShift action_223
action_252 (91) = happyShift action_224
action_252 (17) = happyGoto action_220
action_252 (45) = happyGoto action_302
action_252 _ = happyReduce_125

action_253 _ = happyReduce_134

action_254 (86) = happyShift action_301
action_254 _ = happyFail

action_255 (88) = happyShift action_300
action_255 _ = happyReduce_130

action_256 (53) = happyShift action_38
action_256 (77) = happyShift action_222
action_256 (85) = happyShift action_223
action_256 (91) = happyShift action_224
action_256 (17) = happyGoto action_220
action_256 (45) = happyGoto action_299
action_256 _ = happyReduce_131

action_257 _ = happyReduce_123

action_258 _ = happyReduce_126

action_259 _ = happyReduce_71

action_260 _ = happyReduce_46

action_261 (86) = happyShift action_298
action_261 _ = happyFail

action_262 (86) = happyShift action_297
action_262 _ = happyFail

action_263 (86) = happyShift action_296
action_263 _ = happyFail

action_264 (84) = happyShift action_295
action_264 _ = happyFail

action_265 (84) = happyShift action_294
action_265 _ = happyFail

action_266 (84) = happyShift action_293
action_266 _ = happyFail

action_267 (80) = happyShift action_292
action_267 _ = happyFail

action_268 _ = happyReduce_57

action_269 (90) = happyShift action_291
action_269 _ = happyFail

action_270 (66) = happyShift action_290
action_270 _ = happyFail

action_271 (53) = happyShift action_38
action_271 (54) = happyShift action_89
action_271 (57) = happyShift action_90
action_271 (63) = happyShift action_91
action_271 (65) = happyShift action_92
action_271 (73) = happyShift action_93
action_271 (74) = happyShift action_94
action_271 (75) = happyShift action_95
action_271 (76) = happyShift action_96
action_271 (77) = happyShift action_97
action_271 (78) = happyShift action_98
action_271 (79) = happyShift action_99
action_271 (83) = happyShift action_100
action_271 (85) = happyShift action_101
action_271 (87) = happyShift action_102
action_271 (91) = happyShift action_52
action_271 (95) = happyShift action_53
action_271 (96) = happyShift action_54
action_271 (97) = happyShift action_55
action_271 (99) = happyShift action_56
action_271 (100) = happyShift action_57
action_271 (102) = happyShift action_103
action_271 (103) = happyShift action_104
action_271 (17) = happyGoto action_77
action_271 (20) = happyGoto action_78
action_271 (21) = happyGoto action_79
action_271 (23) = happyGoto action_80
action_271 (25) = happyGoto action_81
action_271 (27) = happyGoto action_82
action_271 (30) = happyGoto action_289
action_271 (31) = happyGoto action_84
action_271 (32) = happyGoto action_85
action_271 (33) = happyGoto action_86
action_271 (34) = happyGoto action_87
action_271 (35) = happyGoto action_88
action_271 _ = happyFail

action_272 (53) = happyShift action_38
action_272 (54) = happyShift action_89
action_272 (57) = happyShift action_90
action_272 (63) = happyShift action_91
action_272 (65) = happyShift action_92
action_272 (73) = happyShift action_93
action_272 (74) = happyShift action_94
action_272 (75) = happyShift action_95
action_272 (76) = happyShift action_96
action_272 (77) = happyShift action_97
action_272 (78) = happyShift action_98
action_272 (79) = happyShift action_99
action_272 (83) = happyShift action_100
action_272 (85) = happyShift action_101
action_272 (87) = happyShift action_102
action_272 (91) = happyShift action_52
action_272 (95) = happyShift action_53
action_272 (96) = happyShift action_54
action_272 (97) = happyShift action_55
action_272 (99) = happyShift action_56
action_272 (100) = happyShift action_57
action_272 (102) = happyShift action_103
action_272 (103) = happyShift action_104
action_272 (17) = happyGoto action_77
action_272 (20) = happyGoto action_78
action_272 (21) = happyGoto action_79
action_272 (23) = happyGoto action_80
action_272 (25) = happyGoto action_81
action_272 (27) = happyGoto action_82
action_272 (30) = happyGoto action_288
action_272 (31) = happyGoto action_84
action_272 (32) = happyGoto action_85
action_272 (33) = happyGoto action_86
action_272 (34) = happyGoto action_87
action_272 (35) = happyGoto action_88
action_272 _ = happyFail

action_273 (82) = happyShift action_287
action_273 _ = happyFail

action_274 (89) = happyShift action_285
action_274 (92) = happyShift action_286
action_274 _ = happyReduce_116

action_275 (53) = happyShift action_38
action_275 (17) = happyGoto action_143
action_275 (18) = happyGoto action_197
action_275 (36) = happyGoto action_284
action_275 (37) = happyGoto action_199
action_275 _ = happyReduce_105

action_276 _ = happyReduce_102

action_277 (53) = happyShift action_38
action_277 (54) = happyShift action_89
action_277 (57) = happyShift action_90
action_277 (63) = happyShift action_91
action_277 (65) = happyShift action_92
action_277 (73) = happyShift action_93
action_277 (74) = happyShift action_94
action_277 (75) = happyShift action_95
action_277 (76) = happyShift action_96
action_277 (77) = happyShift action_97
action_277 (78) = happyShift action_98
action_277 (79) = happyShift action_99
action_277 (83) = happyShift action_100
action_277 (85) = happyShift action_101
action_277 (87) = happyShift action_102
action_277 (91) = happyShift action_52
action_277 (95) = happyShift action_53
action_277 (96) = happyShift action_54
action_277 (97) = happyShift action_55
action_277 (99) = happyShift action_56
action_277 (100) = happyShift action_57
action_277 (102) = happyShift action_103
action_277 (103) = happyShift action_104
action_277 (17) = happyGoto action_77
action_277 (20) = happyGoto action_78
action_277 (21) = happyGoto action_79
action_277 (23) = happyGoto action_80
action_277 (25) = happyGoto action_81
action_277 (27) = happyGoto action_82
action_277 (28) = happyGoto action_283
action_277 (29) = happyGoto action_106
action_277 (30) = happyGoto action_107
action_277 (31) = happyGoto action_84
action_277 (32) = happyGoto action_85
action_277 (33) = happyGoto action_86
action_277 (34) = happyGoto action_87
action_277 (35) = happyGoto action_88
action_277 _ = happyFail

action_278 (90) = happyShift action_282
action_278 _ = happyFail

action_279 _ = happyReduce_79

action_280 (53) = happyShift action_38
action_280 (54) = happyShift action_89
action_280 (57) = happyShift action_90
action_280 (63) = happyShift action_91
action_280 (65) = happyShift action_92
action_280 (73) = happyShift action_93
action_280 (74) = happyShift action_94
action_280 (75) = happyShift action_95
action_280 (76) = happyShift action_96
action_280 (77) = happyShift action_97
action_280 (78) = happyShift action_98
action_280 (79) = happyShift action_99
action_280 (83) = happyShift action_100
action_280 (85) = happyShift action_101
action_280 (87) = happyShift action_102
action_280 (91) = happyShift action_52
action_280 (95) = happyShift action_53
action_280 (96) = happyShift action_54
action_280 (97) = happyShift action_55
action_280 (99) = happyShift action_56
action_280 (100) = happyShift action_57
action_280 (102) = happyShift action_103
action_280 (103) = happyShift action_104
action_280 (17) = happyGoto action_77
action_280 (20) = happyGoto action_78
action_280 (21) = happyGoto action_79
action_280 (23) = happyGoto action_80
action_280 (25) = happyGoto action_81
action_280 (27) = happyGoto action_82
action_280 (30) = happyGoto action_281
action_280 (31) = happyGoto action_84
action_280 (32) = happyGoto action_85
action_280 (33) = happyGoto action_86
action_280 (34) = happyGoto action_87
action_280 (35) = happyGoto action_88
action_280 _ = happyFail

action_281 (86) = happyShift action_330
action_281 _ = happyFail

action_282 (53) = happyShift action_38
action_282 (54) = happyShift action_89
action_282 (57) = happyShift action_90
action_282 (63) = happyShift action_91
action_282 (65) = happyShift action_92
action_282 (73) = happyShift action_93
action_282 (74) = happyShift action_94
action_282 (75) = happyShift action_95
action_282 (76) = happyShift action_96
action_282 (77) = happyShift action_97
action_282 (78) = happyShift action_98
action_282 (79) = happyShift action_99
action_282 (83) = happyShift action_100
action_282 (85) = happyShift action_101
action_282 (87) = happyShift action_102
action_282 (91) = happyShift action_52
action_282 (95) = happyShift action_53
action_282 (96) = happyShift action_54
action_282 (97) = happyShift action_55
action_282 (99) = happyShift action_56
action_282 (100) = happyShift action_57
action_282 (102) = happyShift action_103
action_282 (103) = happyShift action_104
action_282 (17) = happyGoto action_77
action_282 (20) = happyGoto action_78
action_282 (21) = happyGoto action_79
action_282 (23) = happyGoto action_80
action_282 (25) = happyGoto action_81
action_282 (27) = happyGoto action_82
action_282 (30) = happyGoto action_329
action_282 (31) = happyGoto action_84
action_282 (32) = happyGoto action_85
action_282 (33) = happyGoto action_86
action_282 (34) = happyGoto action_87
action_282 (35) = happyGoto action_88
action_282 _ = happyFail

action_283 _ = happyReduce_106

action_284 _ = happyReduce_103

action_285 (53) = happyShift action_38
action_285 (77) = happyShift action_222
action_285 (85) = happyShift action_223
action_285 (91) = happyShift action_224
action_285 (17) = happyGoto action_220
action_285 (41) = happyGoto action_328
action_285 (45) = happyGoto action_274
action_285 _ = happyReduce_117

action_286 (53) = happyShift action_38
action_286 (54) = happyShift action_89
action_286 (57) = happyShift action_90
action_286 (63) = happyShift action_91
action_286 (65) = happyShift action_92
action_286 (73) = happyShift action_93
action_286 (74) = happyShift action_94
action_286 (75) = happyShift action_95
action_286 (76) = happyShift action_96
action_286 (77) = happyShift action_97
action_286 (78) = happyShift action_98
action_286 (79) = happyShift action_99
action_286 (83) = happyShift action_100
action_286 (85) = happyShift action_101
action_286 (87) = happyShift action_102
action_286 (91) = happyShift action_52
action_286 (95) = happyShift action_53
action_286 (96) = happyShift action_54
action_286 (97) = happyShift action_55
action_286 (99) = happyShift action_56
action_286 (100) = happyShift action_57
action_286 (102) = happyShift action_103
action_286 (103) = happyShift action_104
action_286 (17) = happyGoto action_77
action_286 (20) = happyGoto action_78
action_286 (21) = happyGoto action_79
action_286 (23) = happyGoto action_80
action_286 (25) = happyGoto action_81
action_286 (27) = happyGoto action_82
action_286 (28) = happyGoto action_327
action_286 (29) = happyGoto action_106
action_286 (30) = happyGoto action_107
action_286 (31) = happyGoto action_84
action_286 (32) = happyGoto action_85
action_286 (33) = happyGoto action_86
action_286 (34) = happyGoto action_87
action_286 (35) = happyGoto action_88
action_286 _ = happyFail

action_287 _ = happyReduce_73

action_288 (86) = happyShift action_326
action_288 _ = happyFail

action_289 (84) = happyShift action_325
action_289 _ = happyFail

action_290 (53) = happyShift action_38
action_290 (54) = happyShift action_89
action_290 (57) = happyShift action_90
action_290 (63) = happyShift action_91
action_290 (65) = happyShift action_92
action_290 (73) = happyShift action_93
action_290 (74) = happyShift action_94
action_290 (75) = happyShift action_95
action_290 (76) = happyShift action_96
action_290 (77) = happyShift action_97
action_290 (78) = happyShift action_98
action_290 (79) = happyShift action_99
action_290 (83) = happyShift action_100
action_290 (85) = happyShift action_101
action_290 (87) = happyShift action_102
action_290 (91) = happyShift action_52
action_290 (95) = happyShift action_53
action_290 (96) = happyShift action_54
action_290 (97) = happyShift action_55
action_290 (99) = happyShift action_56
action_290 (100) = happyShift action_57
action_290 (102) = happyShift action_103
action_290 (103) = happyShift action_104
action_290 (17) = happyGoto action_77
action_290 (20) = happyGoto action_78
action_290 (21) = happyGoto action_79
action_290 (23) = happyGoto action_80
action_290 (25) = happyGoto action_81
action_290 (27) = happyGoto action_82
action_290 (28) = happyGoto action_324
action_290 (29) = happyGoto action_106
action_290 (30) = happyGoto action_107
action_290 (31) = happyGoto action_84
action_290 (32) = happyGoto action_85
action_290 (33) = happyGoto action_86
action_290 (34) = happyGoto action_87
action_290 (35) = happyGoto action_88
action_290 _ = happyFail

action_291 (53) = happyShift action_38
action_291 (54) = happyShift action_89
action_291 (57) = happyShift action_90
action_291 (63) = happyShift action_91
action_291 (65) = happyShift action_92
action_291 (73) = happyShift action_93
action_291 (74) = happyShift action_94
action_291 (75) = happyShift action_95
action_291 (76) = happyShift action_96
action_291 (77) = happyShift action_97
action_291 (78) = happyShift action_98
action_291 (79) = happyShift action_99
action_291 (83) = happyShift action_100
action_291 (85) = happyShift action_101
action_291 (87) = happyShift action_102
action_291 (91) = happyShift action_52
action_291 (95) = happyShift action_53
action_291 (96) = happyShift action_54
action_291 (97) = happyShift action_55
action_291 (99) = happyShift action_56
action_291 (100) = happyShift action_57
action_291 (102) = happyShift action_103
action_291 (103) = happyShift action_104
action_291 (17) = happyGoto action_77
action_291 (20) = happyGoto action_78
action_291 (21) = happyGoto action_79
action_291 (23) = happyGoto action_80
action_291 (25) = happyGoto action_81
action_291 (27) = happyGoto action_82
action_291 (30) = happyGoto action_323
action_291 (31) = happyGoto action_84
action_291 (32) = happyGoto action_85
action_291 (33) = happyGoto action_86
action_291 (34) = happyGoto action_87
action_291 (35) = happyGoto action_88
action_291 _ = happyFail

action_292 _ = happyReduce_97

action_293 _ = happyReduce_51

action_294 _ = happyReduce_54

action_295 _ = happyReduce_56

action_296 _ = happyReduce_50

action_297 _ = happyReduce_53

action_298 _ = happyReduce_55

action_299 _ = happyReduce_136

action_300 (53) = happyShift action_38
action_300 (77) = happyShift action_222
action_300 (85) = happyShift action_223
action_300 (91) = happyShift action_224
action_300 (17) = happyGoto action_252
action_300 (45) = happyGoto action_253
action_300 (46) = happyGoto action_322
action_300 (47) = happyGoto action_255
action_300 (48) = happyGoto action_256
action_300 _ = happyFail

action_301 _ = happyReduce_124

action_302 _ = happyReduce_135

action_303 (53) = happyShift action_38
action_303 (17) = happyGoto action_321
action_303 _ = happyFail

action_304 (53) = happyShift action_38
action_304 (17) = happyGoto action_320
action_304 _ = happyFail

action_305 (82) = happyShift action_319
action_305 (89) = happyShift action_309
action_305 _ = happyFail

action_306 (82) = happyShift action_318
action_306 (89) = happyShift action_309
action_306 _ = happyFail

action_307 (59) = happyShift action_314
action_307 (16) = happyGoto action_317
action_307 _ = happyReduce_31

action_308 (59) = happyShift action_314
action_308 (16) = happyGoto action_316
action_308 _ = happyReduce_31

action_309 (53) = happyShift action_38
action_309 (17) = happyGoto action_36
action_309 (38) = happyGoto action_239
action_309 (39) = happyGoto action_315
action_309 _ = happyReduce_110

action_310 _ = happyReduce_145

action_311 (59) = happyShift action_314
action_311 (16) = happyGoto action_313
action_311 _ = happyReduce_31

action_312 _ = happyReduce_144

action_313 _ = happyReduce_21

action_314 (53) = happyShift action_38
action_314 (17) = happyGoto action_118
action_314 (19) = happyGoto action_336
action_314 _ = happyFail

action_315 _ = happyReduce_109

action_316 _ = happyReduce_23

action_317 _ = happyReduce_25

action_318 (59) = happyShift action_314
action_318 (16) = happyGoto action_335
action_318 _ = happyReduce_31

action_319 (59) = happyShift action_314
action_319 (16) = happyGoto action_334
action_319 _ = happyReduce_31

action_320 _ = happyReduce_132

action_321 _ = happyReduce_133

action_322 _ = happyReduce_129

action_323 (86) = happyShift action_333
action_323 _ = happyFail

action_324 _ = happyReduce_72

action_325 _ = happyReduce_59

action_326 _ = happyReduce_58

action_327 (89) = happyShift action_332
action_327 _ = happyReduce_114

action_328 _ = happyReduce_115

action_329 (86) = happyShift action_331
action_329 _ = happyFail

action_330 (101) = happyReduce_52
action_330 _ = happyReduce_52

action_331 _ = happyReduce_52

action_332 (53) = happyShift action_38
action_332 (77) = happyShift action_222
action_332 (85) = happyShift action_223
action_332 (91) = happyShift action_224
action_332 (17) = happyGoto action_220
action_332 (41) = happyGoto action_337
action_332 (45) = happyGoto action_274
action_332 _ = happyReduce_117

action_333 _ = happyReduce_60

action_334 _ = happyReduce_22

action_335 _ = happyReduce_24

action_336 _ = happyReduce_32

action_337 _ = happyReduce_113

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
	(HappyAbsSyn40  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.DataDecl happy_var_2 A.NotSized A.Ind happy_var_3 happy_var_5 (reverse happy_var_7) happy_var_9
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 10 8 happyReduction_22
happyReduction_22 ((HappyAbsSyn16  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_4) `HappyStk`
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
	(HappyAbsSyn40  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.DataDecl happy_var_2 A.NotSized A.CoInd happy_var_3 happy_var_5 (reverse happy_var_7) happy_var_9
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 10 10 happyReduction_24
happyReduction_24 ((HappyAbsSyn16  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_4) `HappyStk`
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
	(HappyAbsSyn39  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.RecordDecl happy_var_2 happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 12 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn41  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.FunDecl A.Ind happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 5 13 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn41  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_2) `HappyStk`
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
	(HappyAbsSyn38  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C.LetDecl False happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 5 15 happyReduction_30
happyReduction_30 ((HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_3) `HappyStk`
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
	(HappyAbsSyn41  happy_var_4) `HappyStk`
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
happyReduction_76 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  31 happyReduction_77
happyReduction_77 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn28
		 (C.Quant A.Sigma happy_var_1 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  32 happyReduction_78
happyReduction_78 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn25
		 (C.TBind (Dec Default) {- A.defaultDec -} [] happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  32 happyReduction_79
happyReduction_79 _
	(HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (C.TBind A.irrelevantDec [] happy_var_2
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_2  32 happyReduction_80
happyReduction_80 (HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn25
		 (C.TBind (Dec happy_var_1) [] happy_var_2
	)
happyReduction_80 _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  32 happyReduction_81
happyReduction_81 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  32 happyReduction_82
happyReduction_82 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn25
		 (C.TMeasure happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  32 happyReduction_83
happyReduction_83 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn25
		 (C.TBound happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  33 happyReduction_84
happyReduction_84 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn28
		 (let (f : args) = reverse happy_var_1 in
                if null args then f else C.App f args
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_2  33 happyReduction_85
happyReduction_85 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (C.CoSet happy_var_2
	)
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  33 happyReduction_86
happyReduction_86 _
	 =  HappyAbsSyn28
		 (C.Set C.Zero
	)

happyReduce_87 = happySpecReduce_2  33 happyReduction_87
happyReduction_87 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (C.Set happy_var_2
	)
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  33 happyReduction_88
happyReduction_88 (HappyAbsSyn28  happy_var_3)
	_
	(HappyTerminal (T.Number happy_var_1 _))
	 =  HappyAbsSyn28
		 (let n = read happy_var_1 in
                                            if n==0 then C.Zero else
                                            iterate (C.Plus happy_var_3) happy_var_3 !! (n-1)
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  34 happyReduction_89
happyReduction_89 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_2  34 happyReduction_90
happyReduction_90 (HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_1
	)
happyReduction_90 _ _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  34 happyReduction_91
happyReduction_91 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (C.Proj happy_var_3 : happy_var_1
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_2  34 happyReduction_92
happyReduction_92 _
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (C.Set C.Zero : happy_var_1
	)
happyReduction_92 _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  35 happyReduction_93
happyReduction_93 _
	 =  HappyAbsSyn28
		 (C.Size
	)

happyReduce_94 = happySpecReduce_1  35 happyReduction_94
happyReduction_94 _
	 =  HappyAbsSyn28
		 (C.Max
	)

happyReduce_95 = happySpecReduce_1  35 happyReduction_95
happyReduction_95 _
	 =  HappyAbsSyn28
		 (C.Infty
	)

happyReduce_96 = happySpecReduce_1  35 happyReduction_96
happyReduction_96 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn28
		 (C.Ident happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happyReduce 5 35 happyReduction_97
happyReduction_97 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (C.Sing happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_98 = happySpecReduce_3  35 happyReduction_98
happyReduction_98 _
	(HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (happy_var_2
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  35 happyReduction_99
happyReduction_99 _
	 =  HappyAbsSyn28
		 (C.Unknown
	)

happyReduce_100 = happySpecReduce_2  35 happyReduction_100
happyReduction_100 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (C.Succ happy_var_2
	)
happyReduction_100 _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1  35 happyReduction_101
happyReduction_101 (HappyTerminal (T.Number happy_var_1 _))
	 =  HappyAbsSyn28
		 (iterate C.Succ C.Zero !! (read happy_var_1)
	)
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happyReduce 4 35 happyReduction_102
happyReduction_102 (_ `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (C.Record happy_var_3
	) `HappyStk` happyRest

happyReduce_103 = happySpecReduce_3  36 happyReduction_103
happyReduction_103 (HappyAbsSyn36  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1 : happy_var_3
	)
happyReduction_103 _ _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_1  36 happyReduction_104
happyReduction_104 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 ([happy_var_1]
	)
happyReduction_104 _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_0  36 happyReduction_105
happyReduction_105  =  HappyAbsSyn36
		 ([]
	)

happyReduce_106 = happySpecReduce_3  37 happyReduction_106
happyReduction_106 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn37
		 ((happy_var_1,happy_var_3)
	)
happyReduction_106 _ _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  38 happyReduction_107
happyReduction_107 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn38
		 (C.TypeSig happy_var_1 happy_var_3
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  39 happyReduction_108
happyReduction_108 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  40 happyReduction_109
happyReduction_109 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_3 : happy_var_1
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_2  40 happyReduction_110
happyReduction_110 _
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_110 _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  40 happyReduction_111
happyReduction_111 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_0  40 happyReduction_112
happyReduction_112  =  HappyAbsSyn40
		 ([]
	)

happyReduce_113 = happyReduce 5 41 happyReduction_113
happyReduction_113 ((HappyAbsSyn41  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn45  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 ((C.Clause Nothing [happy_var_1] (Just happy_var_3)) : happy_var_5
	) `HappyStk` happyRest

happyReduce_114 = happySpecReduce_3  41 happyReduction_114
happyReduction_114 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn41
		 ((C.Clause Nothing [happy_var_1] (Just happy_var_3)) : []
	)
happyReduction_114 _ _ _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_3  41 happyReduction_115
happyReduction_115 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn41
		 ((C.Clause Nothing [happy_var_1] Nothing) : happy_var_3
	)
happyReduction_115 _ _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  41 happyReduction_116
happyReduction_116 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn41
		 ((C.Clause Nothing [happy_var_1] Nothing) : []
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_0  41 happyReduction_117
happyReduction_117  =  HappyAbsSyn41
		 ([]
	)

happyReduce_118 = happyReduce 4 42 happyReduction_118
happyReduction_118 ((HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_2) `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (C.Clause (Just happy_var_1) happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_119 = happySpecReduce_2  42 happyReduction_119
happyReduction_119 (HappyAbsSyn43  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn42
		 (C.Clause (Just happy_var_1) happy_var_2 Nothing
	)
happyReduction_119 _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  43 happyReduction_120
happyReduction_120 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (reverse happy_var_1
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_0  44 happyReduction_121
happyReduction_121  =  HappyAbsSyn43
		 ([]
	)

happyReduce_122 = happySpecReduce_2  44 happyReduction_122
happyReduction_122 (HappyAbsSyn45  happy_var_2)
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_2 : happy_var_1
	)
happyReduction_122 _ _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_2  45 happyReduction_123
happyReduction_123 _
	_
	 =  HappyAbsSyn45
		 (C.AbsurdP
	)

happyReduce_124 = happySpecReduce_3  45 happyReduction_124
happyReduction_124 _
	(HappyAbsSyn45  happy_var_2)
	_
	 =  HappyAbsSyn45
		 (happy_var_2
	)
happyReduction_124 _ _ _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_1  45 happyReduction_125
happyReduction_125 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn45
		 (C.IdentP happy_var_1
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_2  45 happyReduction_126
happyReduction_126 (HappyAbsSyn45  happy_var_2)
	_
	 =  HappyAbsSyn45
		 (C.SuccP happy_var_2
	)
happyReduction_126 _ _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_2  45 happyReduction_127
happyReduction_127 _
	_
	 =  HappyAbsSyn45
		 (C.DotP (C.Set C.Zero)
	)

happyReduce_128 = happySpecReduce_2  45 happyReduction_128
happyReduction_128 (HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn45
		 (C.DotP happy_var_2
	)
happyReduction_128 _ _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_3  46 happyReduction_129
happyReduction_129 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (C.PairP happy_var_1 happy_var_3
	)
happyReduction_129 _ _ _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_1  46 happyReduction_130
happyReduction_130 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1
	)
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_1  47 happyReduction_131
happyReduction_131 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn45
		 (let (c, ps) = happy_var_1 in C.ConP c (reverse ps)
	)
happyReduction_131 _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_3  47 happyReduction_132
happyReduction_132 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn45
		 (C.SizeP happy_var_1 happy_var_3
	)
happyReduction_132 _ _ _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_3  47 happyReduction_133
happyReduction_133 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn45
		 (C.SizeP happy_var_3 happy_var_1
	)
happyReduction_133 _ _ _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_1  47 happyReduction_134
happyReduction_134 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1
	)
happyReduction_134 _  = notHappyAtAll 

happyReduce_135 = happySpecReduce_2  48 happyReduction_135
happyReduction_135 (HappyAbsSyn45  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn48
		 ((happy_var_1, [happy_var_2])
	)
happyReduction_135 _ _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_2  48 happyReduction_136
happyReduction_136 (HappyAbsSyn45  happy_var_2)
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (let (c, ps) = happy_var_1 in (c, happy_var_2 : ps)
	)
happyReduction_136 _ _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_1  49 happyReduction_137
happyReduction_137 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn41
		 (reverse happy_var_1
	)
happyReduction_137 _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_3  50 happyReduction_138
happyReduction_138 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_3 : happy_var_1
	)
happyReduction_138 _ _ _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_2  50 happyReduction_139
happyReduction_139 _
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1
	)
happyReduction_139 _ _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_1  50 happyReduction_140
happyReduction_140 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn50
		 ([happy_var_1]
	)
happyReduction_140 _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_0  50 happyReduction_141
happyReduction_141  =  HappyAbsSyn50
		 ([]
	)

happyReduce_142 = happyReduce 5 51 happyReduction_142
happyReduction_142 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind A.paramDec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_143 = happyReduce 5 51 happyReduction_143
happyReduction_143 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind A.irrelevantDec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_144 = happyReduce 6 51 happyReduction_144
happyReduction_144 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind (Dec happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_145 = happyReduce 6 51 happyReduction_145
happyReduction_145 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TBind (Dec SPos) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_146 = happyReduce 4 51 happyReduction_146
happyReduction_146 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (C.TSized happy_var_3
	) `HappyStk` happyRest

happyReduce_147 = happySpecReduce_0  52 happyReduction_147
happyReduction_147  =  HappyAbsSyn52
		 ([]
	)

happyReduce_148 = happySpecReduce_2  52 happyReduction_148
happyReduction_148 (HappyAbsSyn52  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn52
		 (happy_var_1 : happy_var_2
	)
happyReduction_148 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 104 104 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T.Id happy_dollar_dollar _ -> cont 53;
	T.Number happy_dollar_dollar _ -> cont 54;
	T.Data _ -> cont 55;
	T.CoData _ -> cont 56;
	T.Record _ -> cont 57;
	T.Sized _ -> cont 58;
	T.Fields _ -> cont 59;
	T.Mutual _ -> cont 60;
	T.Fun _ -> cont 61;
	T.CoFun _ -> cont 62;
	T.Case _ -> cont 63;
	T.Def _ -> cont 64;
	T.Let _ -> cont 65;
	T.In _ -> cont 66;
	T.Eval _ -> cont 67;
	T.Fail _ -> cont 68;
	T.Check _ -> cont 69;
	T.TrustMe _ -> cont 70;
	T.Impredicative _ -> cont 71;
	T.Type _ -> cont 72;
	T.Set _ -> cont 73;
	T.CoSet _ -> cont 74;
	T.Size _ -> cont 75;
	T.Infty _ -> cont 76;
	T.Succ _ -> cont 77;
	T.Max _ -> cont 78;
	T.AngleOpen _ -> cont 79;
	T.AngleClose _ -> cont 80;
	T.BrOpen _ -> cont 81;
	T.BrClose _ -> cont 82;
	T.BracketOpen _ -> cont 83;
	T.BracketClose _ -> cont 84;
	T.PrOpen _ -> cont 85;
	T.PrClose _ -> cont 86;
	T.Bar _ -> cont 87;
	T.Comma _ -> cont 88;
	T.Sem _ -> cont 89;
	T.Col _ -> cont 90;
	T.Dot _ -> cont 91;
	T.Arrow _ -> cont 92;
	T.Leq _ -> cont 93;
	T.Eq _ -> cont 94;
	T.PlusPlus _ -> cont 95;
	T.Plus _ -> cont 96;
	T.Minus _ -> cont 97;
	T.Slash _ -> cont 98;
	T.Times _ -> cont 99;
	T.Hat _ -> cont 100;
	T.Amp _ -> cont 101;
	T.Lam _ -> cont 102;
	T.Underscore _ -> cont 103;
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
