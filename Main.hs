-- | this module contains an interactive program that can be used to explore the semantics of a WHILE program entered into the console
module Main where

import Data
import Parser
import WSKEA
import ShowZ

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
			printAST ast
			analyseAST ast

printAST :: AST -> IO ()
printAST ast = do
	putStrLn "AST:"
	putStrLn $ showAST ast

analyseAST :: AST -> IO ()
analyseAST ast = do
	putStrLn "enter user input: example: [EZahl n, EBool b, ...]"
	putStr "> "
	input <- getLine
	let z0' = z0 ast (read input)
	let listZ = iterate (>>= delta) (Just z0') -- :: [Maybe Z]
	exploreListZ listZ
	return ()

exploreListZ :: [Maybe Z] -> IO ()
exploreListZ listZ = do
	putStrLn $ unlines $ map showMaybeZ $ [0..] `zip` (takeWhile isJust $ listZ)
	where
		showMaybeZ (index, maybeZ) =
			"delta^" ++ show index ++ ":\n" ++
			case maybeZ of
				Nothing -> show (Nothing :: Maybe Z) ++ "\n"
				Just z -> showZ z
			++ "-----------------------------------------"
			

astTest = case astFromString "if 3>2 then skip else skip" of
	Right ast -> ast

pTest = "if 3>2 then skip else skip" 
