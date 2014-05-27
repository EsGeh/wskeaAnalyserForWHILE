{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module WSKEA where

--import Parser

import Data


--iterate delta z0

-- 2. Def. der Zustandsüberführungsfunktion delta
delta :: Z -> Maybe Z
delta z = case z of
	-- TERM:
	-- Zahl -> value stack
	(w, s, (KTerm (ZahlTerm n): k) , input, output) ->
		Just $ (ZahlWert n : w, s, k, input, output)
	-- s id -> value stack
	(w, s, (KTerm (IdTerm id): k), input, output) ->
		case s id of
			Nothing -> Nothing
			Just zahl -> Just $ (ZahlWert zahl : w, s, k, input, output)
	-- read
	(w, s, KTerm ReadTerm : k, input, output) -> 
		case input of
			((EZahl i): inputRest) -> Just $ (ZahlWert i : w, s, k, inputRest, output)
	-- t op t
	(w, s, KTerm (OPTerm t1 op t2) :k, input, output) -> 
		Just $ (w, s, (KTerm t1 : KTerm t2 : KOp op : k), input, output)
	-- ...
	(ZahlWert n2 : ZahlWert n1 : w, s, KOp op : k, input, output) ->
		Just $ (ZahlWert n : w , s, k, input, output)
			where n = case op of
				Add -> n1 + n2
				Sub -> n1 - n2
				Mul -> n1 * n2
				Div -> floor $ (fromIntegral n1) / (fromIntegral n2)
				Mod -> n1 `mod` n2
	-- COM:
	-- skip
	(w, s, KCom SkipCom : k, input, output) -> 
		Just $ (w, s, k, input, output)
	-- Assign
	(w, s, KCom (AssignCom id t) : k, input, output) ->
		Just $ (w, s, KTerm t : KSym Assign : KId id : k, input, output)
	-- ...
	(ZahlWert n : w, s, KSym Assign : KId id : k, input, output) ->
		Just (w, newS, k, input, output)
			where newS x =
				if x == id
				then Just n
				else s x
	-- Seq
	(w, s, KCom (SeqCom c1 c2) : k, input, output) -> 
		Just $ (w, s, KCom c1 : KCom c2 : k, input, output)
	-- If
	(w, s, KCom (IfCom b c1 c2) : k, input, output) ->
		Just $ (w, s, KBt b : KSym If : KCom c1 : KCom c2 : k, input, output)
	-- ...
	(BoolWert True: w, s, KSym If : KCom c1 : KCom c2 : k, input, output) -> 
		Just $ (w, s, KCom c1 : k, input, output)
	(BoolWert False: w, s, KSym If : KCom c1 : KCom c2 : k, input, output) -> 
		Just $ (w, s, KCom c2 : k, input, output)
	{-(w, s, KBt (BoolBT True) : KSym If : KCom c1 : KCom c2 : k, input, output) -> 
		Just $ (w, s, KCom c1 : k, input, output)
	(w, s, KBt (BoolBT False) : KSym If : KCom c1 : KCom c2 : k, input, output) -> 
		Just $ (w, s, KCom c2 : k, input, output)-}
	-- while
	(w, s, KCom (WhileCom b c): k, input, output) ->
		Just $ (w, s, KBt b : KSym While : KBt b : KCom c : k , input, output)
	-- ...
	((BoolWert True): w, s, (KSym While): b : c: k, input, output) ->
		Just $ (w, s, c : b : (KSym While) : b : c : k, input, output)
	((BoolWert False): w, s, (KSym While): b : c: k, input, output) ->
		Just $ (w, s, k, input, output)
	-- output:
	(w, s, KCom (OutputTerm t) : k, input, output) ->
		Just $ (w, s, KTerm t : (KSym Output) : k, input, output)
	((ZahlWert z) : w, s, KSym Output : k, input, output) ->
		Just $ (w, s, k, input, AZahl z: output)
	(w, s, KCom (OutputBT b) : k, input, output) ->
		Just $ (w, s, KBt b : (KSym Output) : k, input, output)
	((BoolWert b) : w, s, KSym Output : k, input, output) ->
		Just $ (w, s, k, input, ABool b: output)

	-- BT
	(w, s, (KBt (BoolBT b)) : k, input, output) ->
		Just $ (BoolWert b : w, s, k, input, output)
	--(w, s, k, input, output) ->
	_ -> Nothing
		
-- 3. Def. eines Anfangszustandes z0
z0 :: AST -> [E] -> Z
z0 ast input = (
	[],		-- W ValueStack
	const Nothing,	-- S Memory
	[KCom ast],		-- K ControlStack
	input,		-- E user input
	[])		-- A user output

showZ :: Z -> String
showZ (w, s, k, i, o) = unlines $ [
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

-- 4. semantik: 
{-
semantik :: AST -> E -> Maybe A
semantik speicher eingabe = case delta 
l-}
