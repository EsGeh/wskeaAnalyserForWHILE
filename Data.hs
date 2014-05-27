module Data where

import SGData


-- 1. Def. des Zustandsraumes Z:
type Z = ([W], S, [K], [E], [A])

data W = ZahlWert Zahl | BoolWert Bool
	deriving( Show )

type S = (Id -> Maybe Zahl)

data K = KTerm Term 	-- T -> ...
	| KBt BT 	-- B -> ...
	| KCom COM 	-- C -> ...
	| KOp OP	-- Add | Sub | Mul | Div | Mod
	| KBop BOP 	-- LessThan | GreaterThan | Equal | LessOrEqual | GreaterOrEqual | NotEqual
	| KSym KontrollSym -- While | Assign | If 
	| KId Id
	deriving( Show )

data E = EZahl Zahl | EBool Bool
	deriving( Show, Read )

data A = AZahl Zahl | ABool Bool
	deriving( Show )

-- a valid abstract syntax tree is one which head is a command:
type AST = COM

-- 0. primitive types:
type Id = String
type Zahl = Int
data OP = 	-- OP -> + | - | ...
	Add | Sub | Mul | Div | Mod 
	deriving( Show )
data BOP = 	-- BOP -> < | > | ...
	LessThan | GreaterThan | Equal | LessOrEqual | GreaterOrEqual | NotEqual
	deriving( Show )
data UnaryBOP = Not
	deriving( Show )

-- 1. B -> ...
data BT = BoolBT Bool | BOpBT Term BOP Term | UnaryOpBT UnaryBOP BT | ReadBT
	deriving( Show )
-- 2. T -> ...
data Term = OPTerm Term OP Term | ZahlTerm Zahl | IdTerm Id | ReadTerm
	deriving( Show )
-- 3. C -> ...
data COM = SkipCom | AssignCom Id Term | SeqCom COM COM 
	| IfCom BT COM COM
	| WhileCom BT COM
	| OutputTerm Term
	| OutputBT BT
	deriving( Show )


data KontrollSym = While | Assign | If | Output
	deriving( Show )

{-
data UntypedEntry = UntypedEntry {
	entryType :: String }
-}


untypedFromCOM :: COM -> Node String
untypedFromCOM com = case com of
	SkipCom -> leaf "skip"
	AssignCom id t -> node ":=" $ [ leaf id, (untypedFromTERM t)]
	SeqCom c1 c2 -> node ";" $ [ untypedFromCOM c1, untypedFromCOM c2 ]
	IfCom b c1 c2 -> node "if" $ [ untypedFromB b, untypedFromCOM c1, untypedFromCOM c2 ]
	WhileCom b c -> node "while" $ [ untypedFromB b, untypedFromCOM c ]
	OutputTerm t -> node "outputT" $ [ untypedFromTERM t ]
	OutputBT t -> node "outputB" $ [ untypedFromB t ]

untypedFromTERM :: Term -> Node String
untypedFromTERM t = case t of
	OPTerm t1 op t2 -> node (show op) $ [ untypedFromTERM t1, untypedFromTERM t2 ]
	ZahlTerm z -> leaf $ show z
	IdTerm id -> leaf $ id
	ReadTerm -> leaf "readT"

untypedFromB :: BT -> Node String
untypedFromB b = case b of
	BoolBT b -> leaf $ show b
	UnaryOpBT unary b -> node (show unary) $ [ untypedFromB b ]
	ReadBT -> leaf "readB"
	BOpBT t1 bop t2 -> node (show bop) [ untypedFromTERM t1, untypedFromTERM t2 ]
