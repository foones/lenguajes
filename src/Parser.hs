{-# OPTIONS_GHC -w #-}
module Parser where

import AST(
         Id, Type(..), ProgramT(..), FunctionT(..), ParameterT(..),
         BlockT(..), StmtT(..), ExprT(..)
       )
import Lexer(Token(..))
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22

action_0 (51) = happyShift action_4
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (57) = happyAccept
action_2 _ = happyFail

action_3 (51) = happyShift action_4
action_3 (4) = happyGoto action_6
action_3 (5) = happyGoto action_3
action_3 _ = happyReduce_1

action_4 (23) = happyShift action_5
action_4 _ = happyFail

action_5 (25) = happyShift action_8
action_5 (6) = happyGoto action_7
action_5 _ = happyFail

action_6 _ = happyReduce_2

action_7 (30) = happyShift action_14
action_7 (33) = happyShift action_15
action_7 (11) = happyGoto action_13
action_7 _ = happyFail

action_8 (23) = happyShift action_12
action_8 (7) = happyGoto action_9
action_8 (8) = happyGoto action_10
action_8 (9) = happyGoto action_11
action_8 _ = happyReduce_6

action_9 (26) = happyShift action_28
action_9 _ = happyFail

action_10 _ = happyReduce_7

action_11 (27) = happyShift action_27
action_11 _ = happyReduce_8

action_12 (33) = happyShift action_26
action_12 _ = happyFail

action_13 _ = happyReduce_3

action_14 (23) = happyShift action_22
action_14 (52) = happyShift action_23
action_14 (55) = happyShift action_24
action_14 (56) = happyShift action_25
action_14 (12) = happyGoto action_20
action_14 (13) = happyGoto action_21
action_14 _ = happyReduce_15

action_15 (44) = happyShift action_17
action_15 (45) = happyShift action_18
action_15 (46) = happyShift action_19
action_15 (10) = happyGoto action_16
action_15 _ = happyFail

action_16 (30) = happyShift action_14
action_16 (11) = happyGoto action_53
action_16 _ = happyFail

action_17 _ = happyReduce_12

action_18 _ = happyReduce_11

action_19 _ = happyReduce_13

action_20 (31) = happyShift action_52
action_20 _ = happyFail

action_21 (23) = happyShift action_22
action_21 (52) = happyShift action_23
action_21 (55) = happyShift action_24
action_21 (56) = happyShift action_25
action_21 (12) = happyGoto action_51
action_21 (13) = happyGoto action_21
action_21 _ = happyReduce_15

action_22 (25) = happyShift action_48
action_22 (28) = happyShift action_49
action_22 (32) = happyShift action_50
action_22 _ = happyFail

action_23 (23) = happyShift action_38
action_23 (24) = happyShift action_39
action_23 (25) = happyShift action_40
action_23 (28) = happyShift action_41
action_23 (34) = happyShift action_42
action_23 (47) = happyShift action_43
action_23 (48) = happyShift action_44
action_23 (53) = happyShift action_45
action_23 (16) = happyGoto action_47
action_23 (17) = happyGoto action_32
action_23 (18) = happyGoto action_33
action_23 (19) = happyGoto action_34
action_23 (20) = happyGoto action_35
action_23 (21) = happyGoto action_36
action_23 (22) = happyGoto action_37
action_23 _ = happyFail

action_24 (23) = happyShift action_38
action_24 (24) = happyShift action_39
action_24 (25) = happyShift action_40
action_24 (28) = happyShift action_41
action_24 (34) = happyShift action_42
action_24 (47) = happyShift action_43
action_24 (48) = happyShift action_44
action_24 (53) = happyShift action_45
action_24 (16) = happyGoto action_46
action_24 (17) = happyGoto action_32
action_24 (18) = happyGoto action_33
action_24 (19) = happyGoto action_34
action_24 (20) = happyGoto action_35
action_24 (21) = happyGoto action_36
action_24 (22) = happyGoto action_37
action_24 _ = happyFail

action_25 (23) = happyShift action_38
action_25 (24) = happyShift action_39
action_25 (25) = happyShift action_40
action_25 (28) = happyShift action_41
action_25 (34) = happyShift action_42
action_25 (47) = happyShift action_43
action_25 (48) = happyShift action_44
action_25 (53) = happyShift action_45
action_25 (16) = happyGoto action_31
action_25 (17) = happyGoto action_32
action_25 (18) = happyGoto action_33
action_25 (19) = happyGoto action_34
action_25 (20) = happyGoto action_35
action_25 (21) = happyGoto action_36
action_25 (22) = happyGoto action_37
action_25 _ = happyFail

action_26 (44) = happyShift action_17
action_26 (45) = happyShift action_18
action_26 (46) = happyShift action_19
action_26 (10) = happyGoto action_30
action_26 _ = happyFail

action_27 (23) = happyShift action_12
action_27 (8) = happyGoto action_29
action_27 (9) = happyGoto action_11
action_27 _ = happyFail

action_28 _ = happyReduce_5

action_29 _ = happyReduce_9

action_30 _ = happyReduce_10

action_31 (30) = happyShift action_14
action_31 (11) = happyGoto action_77
action_31 _ = happyFail

action_32 (49) = happyShift action_75
action_32 (54) = happyShift action_76
action_32 _ = happyReduce_28

action_33 _ = happyReduce_31

action_34 _ = happyReduce_33

action_35 (35) = happyShift action_67
action_35 (36) = happyShift action_68
action_35 (37) = happyShift action_69
action_35 (38) = happyShift action_70
action_35 (39) = happyShift action_71
action_35 (40) = happyShift action_72
action_35 (41) = happyShift action_73
action_35 (42) = happyShift action_74
action_35 _ = happyReduce_40

action_36 (43) = happyShift action_66
action_36 _ = happyReduce_43

action_37 _ = happyReduce_45

action_38 (25) = happyShift action_64
action_38 (28) = happyShift action_65
action_38 _ = happyReduce_46

action_39 _ = happyReduce_47

action_40 (23) = happyShift action_38
action_40 (24) = happyShift action_39
action_40 (25) = happyShift action_40
action_40 (28) = happyShift action_41
action_40 (34) = happyShift action_42
action_40 (47) = happyShift action_43
action_40 (48) = happyShift action_44
action_40 (53) = happyShift action_45
action_40 (16) = happyGoto action_63
action_40 (17) = happyGoto action_32
action_40 (18) = happyGoto action_33
action_40 (19) = happyGoto action_34
action_40 (20) = happyGoto action_35
action_40 (21) = happyGoto action_36
action_40 (22) = happyGoto action_37
action_40 _ = happyFail

action_41 (23) = happyShift action_38
action_41 (24) = happyShift action_39
action_41 (25) = happyShift action_40
action_41 (28) = happyShift action_41
action_41 (34) = happyShift action_42
action_41 (47) = happyShift action_43
action_41 (48) = happyShift action_44
action_41 (53) = happyShift action_45
action_41 (14) = happyGoto action_62
action_41 (15) = happyGoto action_57
action_41 (16) = happyGoto action_58
action_41 (17) = happyGoto action_32
action_41 (18) = happyGoto action_33
action_41 (19) = happyGoto action_34
action_41 (20) = happyGoto action_35
action_41 (21) = happyGoto action_36
action_41 (22) = happyGoto action_37
action_41 _ = happyReduce_24

action_42 (23) = happyShift action_61
action_42 _ = happyFail

action_43 _ = happyReduce_48

action_44 _ = happyReduce_49

action_45 (23) = happyShift action_38
action_45 (24) = happyShift action_39
action_45 (25) = happyShift action_40
action_45 (28) = happyShift action_41
action_45 (34) = happyShift action_42
action_45 (47) = happyShift action_43
action_45 (48) = happyShift action_44
action_45 (53) = happyShift action_45
action_45 (18) = happyGoto action_60
action_45 (19) = happyGoto action_34
action_45 (20) = happyGoto action_35
action_45 (21) = happyGoto action_36
action_45 (22) = happyGoto action_37
action_45 _ = happyFail

action_46 _ = happyReduce_22

action_47 (30) = happyShift action_14
action_47 (11) = happyGoto action_59
action_47 _ = happyFail

action_48 (23) = happyShift action_38
action_48 (24) = happyShift action_39
action_48 (25) = happyShift action_40
action_48 (28) = happyShift action_41
action_48 (34) = happyShift action_42
action_48 (47) = happyShift action_43
action_48 (48) = happyShift action_44
action_48 (53) = happyShift action_45
action_48 (14) = happyGoto action_56
action_48 (15) = happyGoto action_57
action_48 (16) = happyGoto action_58
action_48 (17) = happyGoto action_32
action_48 (18) = happyGoto action_33
action_48 (19) = happyGoto action_34
action_48 (20) = happyGoto action_35
action_48 (21) = happyGoto action_36
action_48 (22) = happyGoto action_37
action_48 _ = happyReduce_24

action_49 (23) = happyShift action_38
action_49 (24) = happyShift action_39
action_49 (25) = happyShift action_40
action_49 (28) = happyShift action_41
action_49 (34) = happyShift action_42
action_49 (47) = happyShift action_43
action_49 (48) = happyShift action_44
action_49 (53) = happyShift action_45
action_49 (16) = happyGoto action_55
action_49 (17) = happyGoto action_32
action_49 (18) = happyGoto action_33
action_49 (19) = happyGoto action_34
action_49 (20) = happyGoto action_35
action_49 (21) = happyGoto action_36
action_49 (22) = happyGoto action_37
action_49 _ = happyFail

action_50 (23) = happyShift action_38
action_50 (24) = happyShift action_39
action_50 (25) = happyShift action_40
action_50 (28) = happyShift action_41
action_50 (34) = happyShift action_42
action_50 (47) = happyShift action_43
action_50 (48) = happyShift action_44
action_50 (53) = happyShift action_45
action_50 (16) = happyGoto action_54
action_50 (17) = happyGoto action_32
action_50 (18) = happyGoto action_33
action_50 (19) = happyGoto action_34
action_50 (20) = happyGoto action_35
action_50 (21) = happyGoto action_36
action_50 (22) = happyGoto action_37
action_50 _ = happyFail

action_51 _ = happyReduce_16

action_52 _ = happyReduce_14

action_53 _ = happyReduce_4

action_54 _ = happyReduce_17

action_55 (29) = happyShift action_96
action_55 _ = happyFail

action_56 (26) = happyShift action_95
action_56 _ = happyFail

action_57 _ = happyReduce_25

action_58 (27) = happyShift action_94
action_58 _ = happyReduce_26

action_59 (50) = happyShift action_93
action_59 _ = happyReduce_19

action_60 _ = happyReduce_32

action_61 _ = happyReduce_51

action_62 (29) = happyShift action_92
action_62 _ = happyFail

action_63 (26) = happyShift action_91
action_63 _ = happyFail

action_64 (23) = happyShift action_38
action_64 (24) = happyShift action_39
action_64 (25) = happyShift action_40
action_64 (28) = happyShift action_41
action_64 (34) = happyShift action_42
action_64 (47) = happyShift action_43
action_64 (48) = happyShift action_44
action_64 (53) = happyShift action_45
action_64 (14) = happyGoto action_90
action_64 (15) = happyGoto action_57
action_64 (16) = happyGoto action_58
action_64 (17) = happyGoto action_32
action_64 (18) = happyGoto action_33
action_64 (19) = happyGoto action_34
action_64 (20) = happyGoto action_35
action_64 (21) = happyGoto action_36
action_64 (22) = happyGoto action_37
action_64 _ = happyReduce_24

action_65 (23) = happyShift action_38
action_65 (24) = happyShift action_39
action_65 (25) = happyShift action_40
action_65 (28) = happyShift action_41
action_65 (34) = happyShift action_42
action_65 (47) = happyShift action_43
action_65 (48) = happyShift action_44
action_65 (53) = happyShift action_45
action_65 (16) = happyGoto action_89
action_65 (17) = happyGoto action_32
action_65 (18) = happyGoto action_33
action_65 (19) = happyGoto action_34
action_65 (20) = happyGoto action_35
action_65 (21) = happyGoto action_36
action_65 (22) = happyGoto action_37
action_65 _ = happyFail

action_66 (23) = happyShift action_38
action_66 (24) = happyShift action_39
action_66 (25) = happyShift action_40
action_66 (28) = happyShift action_41
action_66 (34) = happyShift action_42
action_66 (47) = happyShift action_43
action_66 (48) = happyShift action_44
action_66 (22) = happyGoto action_88
action_66 _ = happyFail

action_67 (23) = happyShift action_38
action_67 (24) = happyShift action_39
action_67 (25) = happyShift action_40
action_67 (28) = happyShift action_41
action_67 (34) = happyShift action_42
action_67 (47) = happyShift action_43
action_67 (48) = happyShift action_44
action_67 (20) = happyGoto action_87
action_67 (21) = happyGoto action_36
action_67 (22) = happyGoto action_37
action_67 _ = happyFail

action_68 (23) = happyShift action_38
action_68 (24) = happyShift action_39
action_68 (25) = happyShift action_40
action_68 (28) = happyShift action_41
action_68 (34) = happyShift action_42
action_68 (47) = happyShift action_43
action_68 (48) = happyShift action_44
action_68 (20) = happyGoto action_86
action_68 (21) = happyGoto action_36
action_68 (22) = happyGoto action_37
action_68 _ = happyFail

action_69 (23) = happyShift action_38
action_69 (24) = happyShift action_39
action_69 (25) = happyShift action_40
action_69 (28) = happyShift action_41
action_69 (34) = happyShift action_42
action_69 (47) = happyShift action_43
action_69 (48) = happyShift action_44
action_69 (20) = happyGoto action_85
action_69 (21) = happyGoto action_36
action_69 (22) = happyGoto action_37
action_69 _ = happyFail

action_70 (23) = happyShift action_38
action_70 (24) = happyShift action_39
action_70 (25) = happyShift action_40
action_70 (28) = happyShift action_41
action_70 (34) = happyShift action_42
action_70 (47) = happyShift action_43
action_70 (48) = happyShift action_44
action_70 (20) = happyGoto action_84
action_70 (21) = happyGoto action_36
action_70 (22) = happyGoto action_37
action_70 _ = happyFail

action_71 (23) = happyShift action_38
action_71 (24) = happyShift action_39
action_71 (25) = happyShift action_40
action_71 (28) = happyShift action_41
action_71 (34) = happyShift action_42
action_71 (47) = happyShift action_43
action_71 (48) = happyShift action_44
action_71 (20) = happyGoto action_83
action_71 (21) = happyGoto action_36
action_71 (22) = happyGoto action_37
action_71 _ = happyFail

action_72 (23) = happyShift action_38
action_72 (24) = happyShift action_39
action_72 (25) = happyShift action_40
action_72 (28) = happyShift action_41
action_72 (34) = happyShift action_42
action_72 (47) = happyShift action_43
action_72 (48) = happyShift action_44
action_72 (20) = happyGoto action_82
action_72 (21) = happyGoto action_36
action_72 (22) = happyGoto action_37
action_72 _ = happyFail

action_73 (23) = happyShift action_38
action_73 (24) = happyShift action_39
action_73 (25) = happyShift action_40
action_73 (28) = happyShift action_41
action_73 (34) = happyShift action_42
action_73 (47) = happyShift action_43
action_73 (48) = happyShift action_44
action_73 (21) = happyGoto action_81
action_73 (22) = happyGoto action_37
action_73 _ = happyFail

action_74 (23) = happyShift action_38
action_74 (24) = happyShift action_39
action_74 (25) = happyShift action_40
action_74 (28) = happyShift action_41
action_74 (34) = happyShift action_42
action_74 (47) = happyShift action_43
action_74 (48) = happyShift action_44
action_74 (21) = happyGoto action_80
action_74 (22) = happyGoto action_37
action_74 _ = happyFail

action_75 (23) = happyShift action_38
action_75 (24) = happyShift action_39
action_75 (25) = happyShift action_40
action_75 (28) = happyShift action_41
action_75 (34) = happyShift action_42
action_75 (47) = happyShift action_43
action_75 (48) = happyShift action_44
action_75 (53) = happyShift action_45
action_75 (18) = happyGoto action_79
action_75 (19) = happyGoto action_34
action_75 (20) = happyGoto action_35
action_75 (21) = happyGoto action_36
action_75 (22) = happyGoto action_37
action_75 _ = happyFail

action_76 (23) = happyShift action_38
action_76 (24) = happyShift action_39
action_76 (25) = happyShift action_40
action_76 (28) = happyShift action_41
action_76 (34) = happyShift action_42
action_76 (47) = happyShift action_43
action_76 (48) = happyShift action_44
action_76 (53) = happyShift action_45
action_76 (18) = happyGoto action_78
action_76 (19) = happyGoto action_34
action_76 (20) = happyGoto action_35
action_76 (21) = happyGoto action_36
action_76 (22) = happyGoto action_37
action_76 _ = happyFail

action_77 _ = happyReduce_21

action_78 _ = happyReduce_30

action_79 _ = happyReduce_29

action_80 (43) = happyShift action_66
action_80 _ = happyReduce_42

action_81 (43) = happyShift action_66
action_81 _ = happyReduce_41

action_82 (41) = happyShift action_73
action_82 (42) = happyShift action_74
action_82 _ = happyReduce_39

action_83 (41) = happyShift action_73
action_83 (42) = happyShift action_74
action_83 _ = happyReduce_38

action_84 (41) = happyShift action_73
action_84 (42) = happyShift action_74
action_84 _ = happyReduce_37

action_85 (41) = happyShift action_73
action_85 (42) = happyShift action_74
action_85 _ = happyReduce_36

action_86 (41) = happyShift action_73
action_86 (42) = happyShift action_74
action_86 _ = happyReduce_35

action_87 (41) = happyShift action_73
action_87 (42) = happyShift action_74
action_87 _ = happyReduce_34

action_88 _ = happyReduce_44

action_89 (29) = happyShift action_101
action_89 _ = happyFail

action_90 (26) = happyShift action_100
action_90 _ = happyFail

action_91 _ = happyReduce_54

action_92 _ = happyReduce_50

action_93 (30) = happyShift action_14
action_93 (11) = happyGoto action_99
action_93 _ = happyFail

action_94 (23) = happyShift action_38
action_94 (24) = happyShift action_39
action_94 (25) = happyShift action_40
action_94 (28) = happyShift action_41
action_94 (34) = happyShift action_42
action_94 (47) = happyShift action_43
action_94 (48) = happyShift action_44
action_94 (53) = happyShift action_45
action_94 (15) = happyGoto action_98
action_94 (16) = happyGoto action_58
action_94 (17) = happyGoto action_32
action_94 (18) = happyGoto action_33
action_94 (19) = happyGoto action_34
action_94 (20) = happyGoto action_35
action_94 (21) = happyGoto action_36
action_94 (22) = happyGoto action_37
action_94 _ = happyFail

action_95 _ = happyReduce_23

action_96 (32) = happyShift action_97
action_96 _ = happyFail

action_97 (23) = happyShift action_38
action_97 (24) = happyShift action_39
action_97 (25) = happyShift action_40
action_97 (28) = happyShift action_41
action_97 (34) = happyShift action_42
action_97 (47) = happyShift action_43
action_97 (48) = happyShift action_44
action_97 (53) = happyShift action_45
action_97 (16) = happyGoto action_102
action_97 (17) = happyGoto action_32
action_97 (18) = happyGoto action_33
action_97 (19) = happyGoto action_34
action_97 (20) = happyGoto action_35
action_97 (21) = happyGoto action_36
action_97 (22) = happyGoto action_37
action_97 _ = happyFail

action_98 _ = happyReduce_27

action_99 _ = happyReduce_20

action_100 _ = happyReduce_53

action_101 _ = happyReduce_52

action_102 _ = happyReduce_18

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 (Program []
	)

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program (happy_var_1 : unProgram happy_var_2)
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyTerminal (T_ID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Function happy_var_2 Unit happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 6 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyTerminal (T_ID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Function happy_var_2 happy_var_5 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  7 happyReduction_6
happyReduction_6  =  HappyAbsSyn7
		 ([]
	)

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (T_ID happy_var_1))
	 =  HappyAbsSyn9
		 (Parameter happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn10
		 (Int
	)

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn10
		 (Bool
	)

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn10
		 (Vec
	)

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Block happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  12 happyReduction_15
happyReduction_15  =  HappyAbsSyn12
		 ([]
	)

happyReduce_16 = happySpecReduce_2  12 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 (HappyAbsSyn16  happy_var_3)
	_
	(HappyTerminal (T_ID happy_var_1))
	 =  HappyAbsSyn13
		 (StmtAssign happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 6 13 happyReduction_18
happyReduction_18 ((HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_ID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (StmtVecAssign happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (StmtIf happy_var_2 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 5 13 happyReduction_20
happyReduction_20 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (StmtIfElse happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_3  13 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (StmtWhile happy_var_2 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  13 happyReduction_22
happyReduction_22 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (StmtReturn happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 4 13 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_ID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (StmtCall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_0  14 happyReduction_24
happyReduction_24  =  HappyAbsSyn14
		 ([]
	)

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  15 happyReduction_26
happyReduction_26 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  15 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  17 happyReduction_29
happyReduction_29 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (ExprAnd happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  17 happyReduction_30
happyReduction_30 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (ExprOr happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  18 happyReduction_32
happyReduction_32 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (ExprNot happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  19 happyReduction_34
happyReduction_34 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (ExprLe happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  19 happyReduction_35
happyReduction_35 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (ExprGe happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  19 happyReduction_36
happyReduction_36 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (ExprLt happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  19 happyReduction_37
happyReduction_37 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (ExprGt happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  19 happyReduction_38
happyReduction_38 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (ExprEq happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  19 happyReduction_39
happyReduction_39 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (ExprNe happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  19 happyReduction_40
happyReduction_40 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  20 happyReduction_41
happyReduction_41 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (ExprAdd happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  20 happyReduction_42
happyReduction_42 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (ExprSub happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  20 happyReduction_43
happyReduction_43 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  21 happyReduction_44
happyReduction_44 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (ExprMul happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  21 happyReduction_45
happyReduction_45 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  22 happyReduction_46
happyReduction_46 (HappyTerminal (T_ID happy_var_1))
	 =  HappyAbsSyn22
		 (ExprVar happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  22 happyReduction_47
happyReduction_47 (HappyTerminal (T_NUM happy_var_1))
	 =  HappyAbsSyn22
		 (ExprConstNum happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  22 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn22
		 (ExprConstBool True
	)

happyReduce_49 = happySpecReduce_1  22 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn22
		 (ExprConstBool False
	)

happyReduce_50 = happySpecReduce_3  22 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (ExprVecMake happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  22 happyReduction_51
happyReduction_51 (HappyTerminal (T_ID happy_var_2))
	_
	 =  HappyAbsSyn22
		 (ExprVecLength happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happyReduce 4 22 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_ID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (ExprVecDeref happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_53 = happyReduce 4 22 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_ID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (ExprCall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_3  22 happyReduction_54
happyReduction_54 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 57 57 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T_ID happy_dollar_dollar -> cont 23;
	T_NUM happy_dollar_dollar -> cont 24;
	T_LPAREN -> cont 25;
	T_RPAREN -> cont 26;
	T_COMMA -> cont 27;
	T_LBRACK -> cont 28;
	T_RBRACK -> cont 29;
	T_LBRACE -> cont 30;
	T_RBRACE -> cont 31;
	T_ASSIGN -> cont 32;
	T_COLON -> cont 33;
	T_HASH -> cont 34;
	T_LE -> cont 35;
	T_GE -> cont 36;
	T_LT -> cont 37;
	T_GT -> cont 38;
	T_EQ -> cont 39;
	T_NE -> cont 40;
	T_PLUS -> cont 41;
	T_MINUS -> cont 42;
	T_TIMES -> cont 43;
	T_BOOL -> cont 44;
	T_INT -> cont 45;
	T_VEC -> cont 46;
	T_TRUE -> cont 47;
	T_FALSE -> cont 48;
	T_AND -> cont 49;
	T_ELSE -> cont 50;
	T_FUN -> cont 51;
	T_IF -> cont 52;
	T_NOT -> cont 53;
	T_OR -> cont 54;
	T_RETURN -> cont 55;
	T_WHILE -> cont 56;
	_ -> happyError' (tk:tks)
	}

happyError_ 57 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
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
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc-8.0.1/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc640_0/ghc_2.h" #-}


































































































































































{-# LINE 8 "<command-line>" #-}
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
