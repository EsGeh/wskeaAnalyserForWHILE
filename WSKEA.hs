module WSKEA where

--import Parser

import Data

-- 2. Def. der Zustandsüberführungsfunktion delta
delta :: Z -> Maybe Z
delta z = case z of
	-- TERM:
	(w, s, (KTerm (ZahlTerm n): k) , input, output) ->
		Just $ (ZahlWert n : w, s, k, input, output)
	(w, s, (KTerm (IdTerm id): k), input, output) ->
		case s id of
			Nothing -> Nothing
			Just zahl -> Just $ (ZahlWert zahl : w, s, k, input, output)
	(w, s, KTerm ReadTerm : k, input, output) -> 
		case input of
			(i: inputRest) -> Just $ (ZahlWert i : w, s, k, inputRest, output)
	(w, s, KTerm (OPTerm t1 op t2) :k, input, output) -> 
		Just $ (w, s, (KTerm t2 : KTerm t2 : KOp op : k), input, output)
	(ZahlWert n1 : ZahlWert n2 : w, s, KOp op : k, input, output) ->
		Just $ (ZahlWert n : w , s, k, input, output)
			where n = case op of
				Add -> n1 + n2
				Sub -> n1 - n2
				Mul -> n1 * n2
				Div -> floor $ (fromIntegral n1) / (fromIntegral n2)
				Mod -> n1 `mod` n2
	-- COM:
	(w, s, KCom SkipCom : k, input, output) -> 
		Just $ (w, s, k, input, output)
	(w, s, KCom (AssignCom id t) : k, input, output) ->
		Just $ (w, s, KTerm t : KSym Assign : KId id : k, input, output)
	(ZahlWert n : w, s, KSym Assign : KId id : k, input, output) ->
		Just (w, newS, k, input, output)
			where newS x =
				if x == id
				then Just n
				else s x
	(w, s, KCom (SeqCom c1 c2) : k, input, output) -> 
		Just $ (w, s, KCom c1 : KCom c2 : k, input, output)
	(w, s, KCom (IfCom b c1 c2) : k, input, output) ->
		Just $ (w, s, KBt b : KSym If : KCom c1 : KCom c2 : k, input, output)
	(w, s, KBt (BoolBT True) : KCom c1 : KCom c2 : k, input, output) -> 
		Just $ (w, s, KCom c1 : k, input, output)
	(w, s, KBt (BoolBT False) : KCom c1 : KCom c2 : k, input, output) -> 
		Just $ (w, s, KCom c2 : k, input, output)
	--(w, s, k, input, output) ->
	_ -> Nothing
		
-- 3. Def. eines Anfangszustandes z0
z0 ast input = (
	[],
	const Nothing,
	ast,
	input,
	[])

-- 4. semantik: 
{-
semantik :: AST -> E -> Maybe A
semantik speicher eingabe = case delta 
-}
