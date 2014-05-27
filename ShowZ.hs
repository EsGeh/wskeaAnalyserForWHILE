{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- |some functions for showing Z, or ASTs in a formatted way -}
module ShowZ(
	formattedShowZ,
	formattedShowAST,
) where

import Data

import SGData(Tree, Node, node, leaf) -- exportiert einige Funktionen, um mit Bäumen zu arbeiten



-- |schöne darstellung eines Zustandes
formattedShowZ :: Z -> String
formattedShowZ (w, s, k, i, o) = unlines $ [
	printSubList "w:" w,
	printSubList "s:" (asList s),
	printSubList "k:" k,
	printSubList "i:" i,
	printSubList "o:" o]

printSubList tag l = 
	tag ++
		case l of
			[] -> ""
			_ -> "\n" ++ unlines (map (\x -> "    " ++ show x) l)

asList s = ['a'..'z'] `zip` (filter (/=Nothing) $ map (s . return) ['a'..'z'])

instance Show S where
	show s = show $ asList s

formattedShowAST = show . untypedFromCOM


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
