--------------------------------------------------------------------
-- Peter Rindal, Mark Alward, Brenn Kucey                         --
-- CS381 - HW1: Syntax                                            --
-- 4/21/2014                                                      --
--------------------------------------------------------------------
-- ex 1.a                                                         --
--------------------------------------------------------------------

data Cmd = Pen Mode 
         | Moveto (Pos, Pos)
         | Def String Pars Cmd
         | Call String Vals
         | Cmdseq Cmd Cmd 
         deriving Show

data Mode = Up 
          | Down
          deriving Show

data Pos = Const Int 
         | Var String
         deriving Show

data Pars = Param String 
          | Params String Pars
		  deriving Show

data Vals = V Int
          | Vs Int Vals
		  deriving Show

-- ex 1.b ----------------------------------------------------------

vector :: Cmd
vector = Def "vector" (Params "x1" 
                      (Params "y1" 
                      (Params "x2" 
                      (Param  "y2" ))))

            (Cmdseq (Pen Up)
            (Cmdseq (Moveto (Var "x1", Var "y1"))
            (Cmdseq (Pen Down)
     				(Moveto (Var "x2", Var "y2"))
             ) ) )
		
-- ex 1.c ----------------------------------------------------------

steps :: Int -> Cmd
steps i 
    | i <= 0 = Cmdseq  (Pen Up) (Moveto (Const 0, Const 0))
    | otherwise =  (Cmdseq  (Pen Up)
                   (Cmdseq  (Moveto (Const i, Const i))
                   (Cmdseq  (Pen Down)
                   (Cmdseq  (Moveto (Const (pred i), Const i))
                   (Cmdseq  (Moveto (Const (pred i), Const (pred i)))
                            (steps (pred i))
                    ) ) ) ) )
                     

--------------------------------------------------------------------
-- ex 2.a                                                         --
--------------------------------------------------------------------

data Circuit = Circ Gates Links

data Gates = Gateseq Int Gatefn Gates 
           | Emptygate

data Links = Linkseq Link Link Links
           | Emptylink

data Gatefn = And
            | Or
            | Xor
            | Not

type Link = (Int, Int)

-- ex 2.b ----------------------------------------------------------

halfadder :: Circuit
halfadder = Circ 
              (Gateseq 1 Xor 
              (Gateseq 2 And 
              Emptygate ))

              (Linkseq (1, 1) (2, 1) 
              (Linkseq (1, 2) (2, 2) 
              Emptylink))

-- ex 2.c ----------------------------------------------------------

pp_gatefn :: Gatefn -> String
pp_gatefn And = "and"
pp_gatefn Or  = "or"
pp_gatefn Xor = "xor"
pp_gatefn Not = "not"

pp_links :: Links -> String
pp_links Emptylink = ""
pp_links (Linkseq (g1, w1) (g2, w2) rest)
            = "from " ++ show g1 ++ "." ++ show w1 ++ " to " 
            ++ show g2 ++ "." ++ show w2 ++ "; " ++ pp_links rest

pp_gates :: Gates -> String
pp_gates Emptygate = ""
pp_gates (Gateseq g gf rest) 
            = show g ++ ":" ++ pp_gatefn gf ++ "; " ++ pp_gates rest

pp_circuit :: Circuit -> String
pp_circuit (Circ gates links) = pp_gates gates ++ pp_links links

--------------------------------------------------------------------
-- ex 3.a														  --
--------------------------------------------------------------------

data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr
          deriving Show

data Op = Add
        | Multiply
        | Negate
        deriving Show

data Exp = Num Int
         | Apply Op [Exp]
         deriving Show

e = Apply Multiply [ Apply Negate [ Apply Add [ Num 4, Num 4 ] ], Num 7]

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
translate (Plus  e1 e2 ) = Apply Add	  [translate e1, translate e2]
translate (Times e1 e2 ) = Apply Multiply [translate e1, translate e2]
translate (Neg   e1    ) = Apply Negate   [translate e1]
