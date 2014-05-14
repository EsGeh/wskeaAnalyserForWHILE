wskeaAnalyserForWHILE
=====================

a simple haskell-program to analyze the semantics of a program written in the language 'WHILE'.

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
