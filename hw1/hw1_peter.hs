--------------------------------------------------------------------
-- Peter Rindal, Mark Alward									  --
-- CS381 - HW1: Syntax											  --
-- 4/21/2014													  --
--------------------------------------------------------------------
-- ex 1.a														  --
--------------------------------------------------------------------

data Cmd = Pen Mode 
		 | Moveto (Pos, Pos)
		 | Def String [ String ] Cmd
		 | Call String [ Int ]
		 | Cmdseq [ Cmd ]
		 deriving show

data Mode = Up 
		  | Down
		  deriving show

data Pos = Const Int 
		 | Var String
		 deriving show

-- ex 1.b ----------------------------------------------------------

vector :: Cmd
vector = Def "vector" [ "x1", "y1", "x2", "y2" ] 
			Cmdseq [ Pen Up ,
					 Moveto (Var "x1", Var "y1"),
					 Pen Down,
					 Moveto (Var "x2", Var "y2") ]
		
-- ex 1.c ----------------------------------------------------------

steps :: Int -> Cmd
steps i 
	| i <= 0 = Cmdseq [ Pen Up,
						Moveto (Const 0, Const 0)]
	| otherwise = Cmdseq [ Pen Up,
						   Moveto (Const i, Const i),
						   Pen Down,
						   Moveto (Const pred i, Const i),
						   Moveto (Const pred i, Const pred i),
						   step pred i] 

--------------------------------------------------------------------
-- ex 2.a														  --
--------------------------------------------------------------------

data Vircuit = Circ Gates Links

data Gates = Gateseq Int Gatefn Gates 
		   | Emptygate

data Links = Linkseq Int Int Int Int Links
		   | Emptylink

data Gatefn = And
			| Or
			| Xor
			| Not

-- ex 2.b ----------------------------------------------------------

halfAdder :: Circuit
halfadder = Circ 
				Gateseq 1 Xor 
				Gateseq 2 And 
				EmptyGate 
				
				Linkseq 1 1 2 1 
				Linkseq 1 2 2 2 
				EmptyLink
				
-- ex 2.c ----------------------------------------------------------

pp_gatefn :: Gatefn -> String
pp_gatefn And = "and"
pp_gatefn Or  = "or"
pp_gatefn Xor = "xor"
pp_gatefn Not = "not"

pp_links :: Links -> String
pp_links Emptylink = ""
pp_links (Linkseq g1 w1 g2 w2 rest)
			= "from " ++ show g1 ++ "." ++ show w1 ++ " to " 
			++ show g2 ++ "." ++ show w2 ++ ";\n" ++ pp_links rest

pp_gates :: Gates -> string
pp_gates EmptyGate = ""
pp_gates (Circ Gateseq g gf rest) 
			= show g ++ ":" ++ pp_gatefn gf ++ ";\n" ++ pp_gates rest

pp_curuit :: Curcuit -> String
pp_curuit (Circ gates links) = pp_gates gates ++ pp_links links

--------------------------------------------------------------------
-- ex 3.a														  --
--------------------------------------------------------------------

data Expr = N Int
		  | Plus Expr Expr
		  | Times Expr Expr
		  | neg Expr
		  deriving show

data Op = Add
		| Multiply
		| Negate
		deriving show

data Exp = Num Int
		 | Apply Op [Exp]
		 deriving show

Apply Multiply [ Apply Negate [ Apply Add [ Num 4, Num 4 ] ], Num 7]

-- ex 3.b ----------------------------------------------------------

{-
    The first form is more precise because it requires that Neg
    have exactly one argument, and that Plus and Times have exactly
    2 arguments. The second form would allow invalid arithmetic
    expressions, such as multiplication with only one argument or
    negation with multiple arguments.

    The second form is more general since it uses one constructor to 
    represent all types of arithmetic expressions. This could make it
    easier to write functions that operate on the abstract syntax tree,
    since all nodes in the tree use the same constructor. It also makes
    it easy to add new operators to the syntax by simply adding a 
    constructor to "Op".

    Finally, the second form provides a more succinct representation
    for repeated add/multiply expressions like 1*2*3:
        Apply Add [Num 1, Num 2, Num 3]   vs.
        Plus (Plus (Num 1) (Num 2)) (Num 3)
    
-}

-- ex 3.c ----------------------------------------------------------

translate :: Expr -> Exp
translate (N i)			 = Num i
translate (Plus  e1 e2 ) = Apply Add	  [translate e1, translate e1]
translate (Times e1 e2 ) = Apply Multiply [translate e1, translate e1]
translate (Neg   e1 e2 ) = Apply Negate   [translate e1, translate e1]
