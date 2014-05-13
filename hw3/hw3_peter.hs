--------------------------------------------------------------------------------------
-- Peter Rindal, Mark Alward, Brenn Kucey                                           --
-- CS381 - HW3: Types                                                               --
-- 5/7/2014                                                                         --
--------------------------------------------------------------------------------------
-- ex 1.a                                                                           --
--------------------------------------------------------------------------------------

type Prog = [Cmd]


data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | INC
         | SWAP
         | POP Int


type Rank = Int
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
rankC (LD i)    = (0,1)
rankC ADD       = (2,1)
rankC MULT      = (2,1)
rankC DUP       = (1,2)
rankC INC       = (1,1)
rankC SWAP      = (2,2)
rankC (POP i)   = (i,0)

rankP :: Prog -> Maybe Rank
rankP  p = rank p 0 

rank :: Prog -> Rank -> Maybe Rank
rank []         r = Just r
rank (cmd:cs)   r = if r >= n then rank cs (r-n+m)
			                  else Nothing
                    where (n,m) = rankC cmd

test1 = [LD 3, DUP, ADD, LD 4, MULT, DUP, SWAP]  -- Just 2, Just [24,24]
test2 = [LD 3, DUP, DUP, DUP, DUP, POP 5]        -- Just 0, Just []
test3 = [LD 3, DUP, INC, SWAP]                   -- Just 2, Just [4,3]

error1 = [LD 1, ADD]
error2 = [LD 1, LD 2, MULT, POP 2]
error3 = [LD 1, SWAP]

--------------------------------------------------------------------------------------
-- ex 1.b                                                                           --
--------------------------------------------------------------------------------------

type Stack = [Int]

semCmd :: Cmd -> Stack -> Stack
semCmd (LD i)  s          = (i:s)
semCmd DUP     vs@(v:_)   = (v:vs)
semCmd ADD     (v1:v2:vs) = (v1+v2:vs)
semCmd MULT    (v1:v2:vs) = (v1*v2:vs)
semCmd INC     (v1:vs)    = (v1+1:vs)
semCmd SWAP    (v1:v2:vs) = (v2:v1:vs)
semCmd (POP 0) s          = s
semCmd (POP i) (_:vs)     = semCmd (POP (i-1)) vs


sem :: Prog -> Stack -> Stack
sem []     s = s
sem (p:ps) s = sem ps (semCmd p s)

stackSafe :: Prog -> Bool
stackSafe p = rankP p /= Nothing

semStatTC :: Prog -> Maybe Stack
semStatTC p | stackSafe p = Just (sem p [])
            | otherwise   = Nothing

--------------------------------------------------------------------------------------
-- ex 2.a                                                                           --
--------------------------------------------------------------------------------------

data Shape = X
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int,Int)

bbox :: Shape -> BBox
bbox X 		    = (1,1)
bbox (TD s1 s2) = (max s1x s2x, s1y + s2y)
                where   (s1x,s1y) = bbox s1
                        (s2x,s2y) = bbox s2
bbox (LR s1 s2) = (s1x + s2x, max s1y s2y)
                where   (s1x,s1y) = bbox s1
                        (s2x,s2y) = bbox s2

test4 = TD (LR X X) X                         -- (2,2)
test5 = TD (TD (LR X X) (TD X X)) (LR X X)    -- (2,4)
test6 = LR (LR X (TD X X)) (LR (LR X X) X)    -- (5,2)

--------------------------------------------------------------------------------------
-- ex 2.b                                                                           --
--------------------------------------------------------------------------------------

rect :: Shape -> Maybe BBox
rect X	        = Just (1,1)
rect (TD s1 s2) =
        case (rect s1       , rect s2       ) of
            (Just (s1x,s1y), Just (s2x,s2y)) -> if s1x == s2x then Just (s1x, s1y+s2y)
                                                              else Nothing
            otherwise 		                 -> Nothing
rect (LR s1 s2) = 
        case (rect s1       , rect s2       ) of
            (Just (s1x,s1y), Just (s2x,s2y)) -> if s1y == s2y then Just (s1x+s2x, s1y)
                                                              else Nothing
            otherwise 		                 -> Nothing  

test7 = TD (LR X X) (LR X X)                 -- Just (2,2)
test8 = LR (LR (TD X X) (TD X X)) (TD X X)   -- Just (3,2)
test9 = LR (LR X X) (LR (LR X X)(LR X X))    -- Just (6,1)
test10 = TD test9 test9                      -- Just (6,2)
test11 = LR test8 test10                     -- Just (9,2)

error4 = TD test7 test8
error5 = LR test9 test10
error6 = TD test8 test11

--------------------------------------------------------------------------------------
-- ex 3.a                                                                           --
--------------------------------------------------------------------------------------
-- |                                                                              | --
-- |(1) f :: [a] -> a -> [a]                                                      | --
-- |    g :: [a] -> b -> [b]                                                      | --
-- |                                                                              | --
-- |(2) Function f can either output x or [y], infering that both [y] and x must  | --
-- |     be of the same type.                                                     | --
-- |    Function g can either output [y] or []. Neither of these include x, so    | --
-- |     the types of x and y may be separate.                                    | --
-- |                                                                              | --
-- |(3) Function g is the more general of the two.  Function g can be called      | --
-- |     such that it's type is the same as f, but not vice versa.                | --
-- |                                                                              | --
-- |(4) As explained in 2), f is constrained to have x's elements and y as the    | --
-- |     same type because of the outputs x and [y].                              | --
-- |    Function g doesn't have this constraint; it either outputs [] or [y].     | --
-- |     This allows y to be a disjoint type.                                     | --
--------------------------------------------------------------------------------------
-- ex 3.b                                                                           --
--------------------------------------------------------------------------------------

-- h :: [b] -> [(a,b)] -> [b]
h bs (ab:abs) | null (fst (unzip abs)) = bs ++ (snd (unzip abs))

-- k :: (a -> b) -> ((a -> b) -> a) -> b
k fab  faba = fab (faba (fab))



















