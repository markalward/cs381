
-- ex 1

data Cmd = Pen Mode
         | MoveTo (Pos, Pos)
         | Def String [String] Cmd
         | Call String [Int]
         | CmdSeq [Cmd]
         deriving Show

data Mode = Up | Down deriving Show
data Pos = Const Int | Var String deriving Show


vector :: Cmd
vector = Def "vector" ["x1", "y1", "x2", "y2"]
            (CmdSeq [Pen Up,
                     MoveTo (Var "x1", Var "y1"),
                     Pen Down,
                     MoveTo (Var "x2", Var "y2"),
                     Pen Up]
            )

steps :: Int -> Cmd
steps 0 = CmdSeq [MoveTo (Const 0,Const 0), Pen Down]
steps i = CmdSeq (prev ++ [MoveTo (Const (i-1), Const (i)), 
                           MoveTo(Const (i), Const(i))])
          where (CmdSeq prev) = steps(i-1)


-- ex 2

 circuit ::= gates; links
gates ::= num:gateFn ; gates j ϵ
gateFn ::= and j or j xor j not
links ::=

data Circuit = Circ Gates Links
data Gates = GateSeq Int GateFn Gates
           | EmptyGate
data GateFn = And | Or | Xor | Not
data Links = LinkSeq (Int, Int) (Int, Int) Links
           | EmptyLink

adder :: Circuit
adder = Circ
            (GateSeq 1 Xor (GateSeq 2 And EmptyGate))
            (LinkSeq (1,1) (2,1) (LinkSeq (1,2) (2,2) EmptyLink)


pr_gatefn :: GateFn -> String
pr_gatefn And = "and"
pr_gatefn Or = "or"
pr_gatefn Xor = "xor"
pr_gatefn Not = "not"

pr_gates :: Gates -> String
pr_gates EmptyGate = ""
pr_gates (GateSeq i fn seq) = show i ++ ":" ++ (pr_gatefn fn) ++ "; "
                              ++ (pr_gates seq)

pr_links :: Links -> String
pr_links EmptyLink = ""
pr_links (LinkSeq (x1,x2) (y1,y2) seq) = "From " ++ 
    show x1 ++ "." ++ show x2 ++ " to " ++ 
    show y1 ++ "." ++ show y2 ++ "; " ++ pr_links seq

pr_circuit :: Circuit -> String
pr_circuit (Circ gates links) = pr_gates gates ++ pr_links links


-- ex 3

data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr

data Op = Add | Multiply | Negate
data Exp = Num Int
         | Apply Op [Exp]

translate :: Expr -> Exp
translate (N a) = Num a
translate (Times a b) = Apply Multiply [translate a, translate b]
translate (Plus a b) = Apply Add [translate a, translate b]
translate (Neg a) = Apply Neg [translate a]

