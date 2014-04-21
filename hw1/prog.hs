
-- 1a

data Cmd = Pen Mode
         | MoveTo (Pos, Pos)
         | Def String Pars Cmd
         | Call String Vals
         | CmdSeq [Cmd]
         deriving Show

data Mode = Up | Down deriving Show
data Pos = Const Int | Var String deriving Show
type Pars = [String]
type Vals = [Int]

-- 1b

{-
    def vector(x1, y1, x2, y2)
        pen up;
        moveto (x1, y1);
        pen down;
        moveto (x2, y2);
        pen up
-}

vector :: Cmd
vector = Def "vector" ["x1", "y1", "x2", "y2"]
            (CmdSeq [Pen Up,
                     MoveTo (Var "x1", Var "y1"),
                     Pen Down,
                     MoveTo (Var "x2", Var "y2"),
                     Pen Up]
            )

-- 1c

steps :: Int -> Cmd
steps 0 = CmdSeq [MoveTo (Const 0,Const 0), Pen Down]
steps i = CmdSeq (prev ++ [MoveTo (Const (i-1), Const (i)), 
                           MoveTo(Const (i), Const(i))])
          where (CmdSeq prev) = steps(i-1)


-- 2a

data Circuit = Circ Gates Links deriving Show
data Gates = GateSeq Int GateFn Gates
           | EmptyGate
           deriving Show
data GateFn = And | Or | Xor | Not deriving Show
data Links = LinkSeq Link Link Links
           | EmptyLink
           deriving Show
type Link = (Int, Int)

-- 2b

adder :: Circuit
adder = Circ
            (GateSeq 1 Xor (GateSeq 2 And EmptyGate))
            (LinkSeq (1,1) (2,1) (LinkSeq (1,2) (2,2) EmptyLink))


-- 2c

pr_gatefn :: GateFn -> String
pr_gatefn And = "and"
pr_gatefn Or = "or"
pr_gatefn Xor = "xor"
pr_gatefn Not = "not"

pr_gates :: Gates -> String
pr_gates EmptyGate = ""
pr_gates (GateSeq i fn seq) = show i ++ ":" ++ (pr_gatefn fn) ++ "; \n"
                              ++ (pr_gates seq)

pr_links :: Links -> String
pr_links EmptyLink = ""
pr_links (LinkSeq (x1,x2) (y1,y2) seq) = "From " ++ 
    show x1 ++ "." ++ show x2 ++ " to " ++ 
    show y1 ++ "." ++ show y2 ++ "; \n" ++ pr_links seq

pr_circuit :: Circuit -> String
pr_circuit (Circ gates links) = pr_gates gates ++ pr_links links


-- 3a

data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr
          deriving Show

data Op = Add | Multiply | Negate deriving Show
data Exp = Num Int
         | Apply Op [Exp]
         deriving Show

-- -(3+4)*7
e = Apply Negate [Apply Multiply [Apply Add [Num 3, Num 4], Num 7]]

-- 3b

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

-- 3c

translate :: Expr -> Exp
translate (N a) = Num a
translate (Times a b) = Apply Multiply [translate a, translate b]
translate (Plus a b) = Apply Add [translate a, translate b]
translate (Neg a) = Apply Negate [translate a]


