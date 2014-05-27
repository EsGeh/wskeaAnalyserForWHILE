module Main where

import Data
import Parser
import WSKEA

import Control.Monad
import Data.Maybe

main = do
	putStrLn "please enter a WHILE-Program:"
	putStr "> "
	p <- getLine
	analyse p


analyse :: String -> IO ()
analyse p = do
	let eitherAST = astFromString p
	case eitherAST of
		Left error -> do
			putStrLn "ERROR:"
			print $  error
		Right ast -> do
			putStrLn "analysation:--------------------------------------"
			showAST ast
			analyseAST ast

showAST :: AST -> IO ()
showAST ast = do
	putStrLn "AST:"
	print $ untypedFromCOM ast

analyseAST :: AST -> IO ()
analyseAST ast = do
	putStrLn "enter user input: example: [EZahl n, EBool b, ...]"
	putStr "> "
	input <- getLine
	let z0' = z0 ast (read input)
	let listZ = iterate (>>= delta) (Just z0') -- :: [Maybe Z]
	exploreListZ listZ
	return ()

exploreListZ listZ = do
	putStrLn $ unlines $ map showMaybeZ $ [0..] `zip` (takeWhile isJust $ listZ)
	where
		showMaybeZ (index, maybeZ) =
			"delta^" ++ show index ++ ":\n" ++
			case maybeZ of
				Nothing -> show (Nothing :: Maybe Z) ++ "\n"
				Just z -> showZ z
			++ "-----------------------------------------"
			

{-
interactWithListZ listZ = sequence_ $ do
	input <- getChar
	return ()
-}
			

astTest = case astFromString "if 3>2 then skip else skip" of
	Right ast -> ast

pTest = "if 3>2 then skip else skip" 
