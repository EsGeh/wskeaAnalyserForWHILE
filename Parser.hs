module Parser(
	astFromString
) where

import Data

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

import Text.Parsec
import Text.Parsec.String
--import Text.Parsec.Prim


astFromString :: String -> Either ParseError AST
astFromString str =
	parse lexer "" str 	-- :: Either ParseError [Token]
	>>=
	parse parseWHILE "" 	-- :: [Token] -> Either ParseError AST


-- a parser that takes Tokens and the context src to calculate a representation repr
type ParseTokStream src repr  = GenParser Token src repr 

parseWHILE :: ParseTokStream () COM
parseWHILE = parseC

-- C -> ...
parseC :: ParseTokStream () COM
parseC =  -- chainl1 solves the left recursion problem:
	chainl1
		parsePrimitiveCommand 	-- defines how to parse C (if it is not a sequence)
		parseSequenceCommand	-- defines how to parse a ';'
{- internally, chainl1 might transform the grammar like this:
	C -> <skip> RC | <if> B <then> C <else> C RC | ...
	RC -> <;> C | <epsilon>
-}

-- try to parse <skip> | <if> B <then> C <else> C | ...
parsePrimitiveCommand :: ParseTokStream () COM
parsePrimitiveCommand = do
	-- try to parse as 'skip'
	input <- getInput
	case input of
		(CmdToken SkipCmd):xs -> do
			setInput xs
			return $ SkipCom
		_ -> fail "expected 'skip'"
	<|> parseIf 	-- try to parse as 'if b then c1 else c2'
	<|> parseWhile	-- ...
	<|> parseOutput
	<|> parseAssignement

-- try to parse a semicolon:
-- returns a binary function to concatenate two COMs
parseSequenceCommand :: ParseTokStream () (COM -> COM -> COM)
parseSequenceCommand = do
	input <- getInput
	case input of
		(CmdToken SeqCmd): xs -> do
			setInput xs
			return $ \s1 s2 -> SeqCom s1 s2
		_ -> fail "expected ';'"
----------------------------------------------------------------------------------------

-- T -> Z | I | <read> | T OP T
parseT :: ParseTokStream () Term
parseT = greedyOp
	parsePrimitiveTerm
	parseOpTerm

greedyOp :: ParseTokStream () Term -> ParseTokStream () (Term -> Term -> Term) -> ParseTokStream () Term
greedyOp prim conc = do
	t1 <- prim
	maybeOp <- optionMaybe (conc)
	case maybeOp of
		Nothing -> return $ t1
		Just op -> do
			t2 <- greedyOp prim conc
			return $ op t1 t2
			
-- try to parse Z | I | <read>
parsePrimitiveTerm :: ParseTokStream () Term
parsePrimitiveTerm = do
	liftM ZahlTerm parseZ <|> liftM IdTerm parseI <|> parseReadTerm 

-- try to parse an OP:
-- returns a binary function to concatenate two Terms
parseOpTerm :: ParseTokStream () (Term -> Term -> Term)
parseOpTerm = do
	op <- parseOP
	return $ \t1 t2 -> OPTerm t1 op t2
----------------------------------------------------------------------------------------

-- B -> <read> | W | <not> B | T <BOP> T
parseB :: ParseTokStream () BT
parseB = do
	parseReadBT <|> parseW <|> parseNot 
	<|> do
		t1 <- parseT
		op <- parseBOP
		t2 <- parseT
		return $ BOpBT t1 op t2


parseOutput :: ParseTokStream () COM
parseOutput = do
	input <- getInput
	case input of
		(CmdToken OutputCmd):xs -> do
			setInput xs
			try $ do
				t <- parseT
				return $ OutputTerm t
			<|> do
				b <- parseB
				return $ OutputBT b
		_ -> fail "expected 'output'"




parseIf :: ParseTokStream () COM
parseIf = do
	input <- getInput
	case input of
		(CmdToken IfCmd): xs -> do
			setInput xs
			b <- parseB
			input' <- getInput
			case input' of
				(CmdToken ThenCmd):xs' -> do
					setInput xs'
					c1 <- parseC
					input'' <- getInput
					case input'' of
						(CmdToken ElseCmd):xs'' -> do
							setInput xs''
							c2 <- parseC
							return $ IfCom b c1 c2
						_ -> fail "expected 'else'"
				_ -> fail "expected 'then'"
		_ -> fail "expected 'if'"

parseWhile :: ParseTokStream () COM
parseWhile = do
	input <- getInput
	case input of
		(CmdToken WhileCmd): xs -> do
			setInput xs
			b <- parseB
			input' <- getInput
			case input' of
				(CmdToken DoCmd):xs' -> do
					setInput xs'
					c <- parseC
					return $ WhileCom b c
				_ -> fail "expected 'do'"
		_ -> fail "expected 'while'"

parseAssignement :: ParseTokStream () COM
parseAssignement = do
	id <- parseI
	input <- getInput
	case input of
		(CmdToken AssignCmd): xs -> do
			setInput xs
			t <- parseT
			return $ AssignCom id t
		_ -> fail "expected '='"



parseI :: ParseTokStream () Id
parseI = tokenPrim show nextPos testTok
	where
		testTok token = case token of
			IdToken id -> Just $ id
			_ -> Nothing
		nextPos pos x xs = incSourceColumn pos 1

parseZ :: ParseTokStream () Zahl
parseZ = tokenPrim show nextPos testTok
	where
		testTok token = case token of
			NumToken n -> Just $ n
			_ -> Nothing
		nextPos pos x xs = incSourceColumn pos 1

parseW :: ParseTokStream () BT
parseW = do
	input <- getInput 
	case input of
		(x:xs) -> case x of
			(BoolToken b) -> do
				setInput xs
				return $ BoolBT b
			_ -> fail "expected 'true' or 'false'"
		[] -> fail "empty Boolean value"
{-
	tokenPrim show nextPos testTok
	where
		testTok token = case token of
			BoolToken b -> Just $ BoolBT $ b
			_ -> Nothing
		nextPos pos x xs = incSourceColumn pos 1
-}

parseOP :: ParseTokStream () OP
parseOP = tokenPrim show nextPos testTok
	where
		testTok token = case token of
			OpToken b -> Just $ b
			_ -> Nothing
		nextPos pos x xs = incSourceColumn pos 1

parseBOP :: ParseTokStream () BOP
parseBOP = tokenPrim show nextPos testTok
	where
		testTok token = case token of
			BOpToken b -> Just $ b
			_ -> Nothing
		nextPos pos x xs = incSourceColumn pos 1

parseReadBT :: ParseTokStream () BT
parseReadBT = {-do
	input <- getInput 
	setInput xs
	case input of
		[] -> fail "'read' expected"
		(x:xs) -> case x of
			(CmdToken ReadCmd) -> do
				return $ ReadBT
			_ -> fail "expected 'read'"
-}
	tokenPrim show nextPos testTok
	where
		testTok token = case token of
			CmdToken ReadCmd -> Just $ ReadBT
			_ -> Nothing
		nextPos pos x xs = incSourceColumn pos 1

parseReadTerm :: ParseTokStream () Term
parseReadTerm = tokenPrim show nextPos testTok
	where
		testTok token = case token of
			CmdToken ReadCmd -> Just $ ReadTerm
			_ -> Nothing
		nextPos pos x xs = incSourceColumn pos 1

parseNot :: ParseTokStream () BT
parseNot = do
	unaryOp <- tokenPrim show nextPos testTok
	bt <- parseB
	return $ UnaryOpBT unaryOp bt
	where
		testTok token = case token of
			UnaryBopToken unaryOp -> Just $ unaryOp
			_ -> Nothing
		nextPos pos x xs = incSourceColumn pos 1


data Token =
	NumToken Int | BoolToken Bool | IdToken String
	| OpToken OP | BOpToken BOP
	| UnaryBopToken UnaryBOP
	| CmdToken CmdType
	deriving( Show )
data CmdType =
	SkipCmd 
	| IfCmd | ThenCmd | ElseCmd
	| WhileCmd | DoCmd 
	| ReadCmd | OutputCmd
	| AssignCmd | SeqCmd
	deriving( Show )


-- chops a String into a list of Tokens
lexer :: GenParser Char () [Token]
lexer = do
	spaces
	tokens <- parseToken `sepEndBy` many space
	eof
	return tokens

parseToken :: GenParser Char () Token
parseToken =
	try parseBoolToken
	<|> try parseNumToken
	<|> try parseCmdToken
	<|> try parseOpToken
	<|> try parseBOpToken
	<|> try parseUnaryBopToken
	<|> parseIdToken

parseUnaryBopToken :: GenParser Char () Token
parseUnaryBopToken = do
	input <- getInput
	case input of
		('n':'o':'t':xs) -> do
			setInput xs
			return $ UnaryBopToken $ Not
		_ -> fail "'not' expected"

parseCmdToken :: GenParser Char () Token
parseCmdToken = do
	input <- getInput
	let list = map (\s -> tryToMatch s input) listKeywords
	case filter isJust list of
		(Just (pattern, xs)): _ -> do
			setInput xs
			return $ tokenFromKeyWord pattern
		[] -> fail $ "expected " ++ foldr1 (\l r -> l ++ ", " ++ r) (map surroundWith listKeywords)
			where surroundWith x = "'" ++ x ++ "'" :: String
	where
		listKeywords = ["skip", "if", "then", "else", "while", "do", "read", "output", ":=", ";"]
		tokenFromKeyWord str = case str of
			"skip" -> CmdToken SkipCmd
			"if" -> CmdToken IfCmd
			"then" -> CmdToken ThenCmd
			"else" -> CmdToken ElseCmd
			"while" -> CmdToken WhileCmd
			"do" -> CmdToken DoCmd
			"read" -> CmdToken ReadCmd
			"output" -> CmdToken OutputCmd
			":=" -> CmdToken AssignCmd
			";" -> CmdToken SeqCmd

tryToMatch pattern str = case stripPrefix pattern str of
	Just rest -> Just (pattern, rest)
	Nothing -> Nothing

parseBoolToken :: GenParser Char () Token
parseBoolToken = do
	str <- (
		string "true"
		<|> string "false")
	return $ case str of
		"true" -> BoolToken True
		"false" -> BoolToken False

parseNumToken :: GenParser Char () Token
parseNumToken = do
	num <- many1 digit
	return $ NumToken $ read num

parseIdToken :: GenParser Char () Token
parseIdToken = do
	(x: xs) <- getInput
	if isLetter x && isLower x
	then do
		setInput xs
		return $ IdToken [x]
	else fail "expected identifier (a..x)"

parseOpToken :: GenParser Char () Token
parseOpToken = do
	(x:xs) <- getInput
	case opFromChar x of
		Just op -> do
			setInput xs
			return $ OpToken op
		Nothing -> case (x:xs) of
			('m':'o':'d':xs') -> do
				setInput xs'
				return $ OpToken Mod
			_ -> fail "expected +, -, *, /, or mod"

parseBOpToken :: GenParser Char () Token
parseBOpToken = do
	input <- getInput
	case bopFromChar input of
		Just (op, xs) -> do
			setInput xs
			return $ BOpToken op
		Nothing -> fail "expected <, >, =, <=, >=, or !="

opFromChar :: Char -> Maybe OP
opFromChar char = case char of
	'+' -> Just Add
	'-' -> Just Sub
	'*' -> Just Mul
	'/' -> Just Div
	_ -> Nothing

bopFromChar :: String -> Maybe (BOP, String)
bopFromChar char = case char of
	'<':'=':xs -> Just (LessOrEqual, xs)
	'>':'=':xs -> Just (GreaterOrEqual, xs)
	'!':'=':xs -> Just (NotEqual, xs)
	'<':xs -> Just (LessThan, xs)
	'>':xs -> Just (GreaterThan, xs)
	'=':xs -> Just (Equal, xs)
	_ -> Nothing
