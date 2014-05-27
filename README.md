wskeaAnalyserForWHILE
=====================

a simple haskell-program to analyze the semantics of a program written in the language 'WHILE'.

## WHILE Syntax

The language WHILE has the following syntax:

1. primitives

		W -> <true> | <false>
		Z -> MIN_INT, .. , -1, 0, 1, .., MAX_INT
		I -> a..z
		OP -> + | - | * | / | <mod>
		BOP -> < | > | = | <= | >= | !=

2. Boolean Expressions

		B ->
			W
			| <not> B
			| <read>
			| T BOP T

3. Number Expressions

		T ->
			Z
			| I
			| <read>
			| T OP T 	(*)

4. Commands

		C ->
			I <:=> T
			| <if> B <then> C <else> C
			| <while> B <do> C
			| <output> T
			| <output> B
			| C <;> C	(*)


Notice that the grammar includes left-recursion in the productions marked with (*)


## Installation

### Prerequisits

1. make shure that you have cabal-install (it usually is installed as a part of the "haskell platform").

2. make shure that you have git installed

### Dependencies

wskeaAnalyserForWHILE has the following dependencies:
	sgCard-1.3.*
	sgData-1.1.*

So, we have to install them first:

### Installing sgCard

	git clone --branch release https://github.com/EsGeh/sgCard.git
	cd sgCard
	git checkout 1.3.0.0
	cabal configure
	cabal build
	cabal install
	cabal haddock

### Installing sgData

	git clone --branch release https://github.com/EsGeh/sgData.git
	cd sgData
	git checkout 1.1.0.0
	cabal configure
	cabal build
	cabal install
	cabal haddock

### installing wskeaAnalyserForWHILE

	git clone https://github.com/EsGeh/wskeaAnalyserForWHILE.git
	cd wskeaAnalyserForWHILE
	cabal configure
	cabal build
	ln -s ./dist/build/wskeaAnalyserForWHILE/wskeaAnalyserForWHILE

## Usage,Examples

execute the program by typing

	./wskeaAnalyserForWHILE [ <program> ]

if no <program> is given, you will be asked to type a WHILE program

after that, enter a list of user inputs (just type '[]', if empty)

### example programs

if 4>3 then output true else output false

x := read ; if x >= 0 then output 1 else output 0 
