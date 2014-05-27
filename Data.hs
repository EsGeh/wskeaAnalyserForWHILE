-- | exportiert Datentypen, die von anderen Modulen benutzt werden
module Data where



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
	| KUnaryBop UnaryBOP
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


