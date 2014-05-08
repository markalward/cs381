--------------------------------------------------------------------
-- Peter Rindal, Mark Alward, Brenn Kucey                         --
-- CS381 - HW3: Types                                             --
-- 5/7/2014                                                       --
--------------------------------------------------------------------
-- ex 1.a                                                         --
--------------------------------------------------------------------

type Prog = [Cmd]


data Cmd = LD Int
	 | ADD
	 | MULT
	 | DUP
	 | IN
	 | SWAP
	 | POP Int


tpye Rank = Int
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
rankC LD i  = (0,1)
rankC ADD   = (2,1)
rankC MULT  = (2,1)
rankC DUP   = (1,2)
rankC INC   = (1,1)
rankC SWAP  = (2,2)
rankC POP i = (i,0)

rankP :: Prog -> Maybe Rank
rankP  p = rank p 0 

rank :: Prog -> Rank -> Maybe Rank
rank []         r = Just r
rank ((n,m):cs) r = if r >= n then Just (rank cs  (r-n)) + m
			    else Nothing

--------------------------------------------------------------------
-- ex 1.b                                                         --
--------------------------------------------------------------------

type Stack = [Int]
type Val   = Int

semCmd :: Cmd -> Stack -> Stack
semCmd (LD i)  s          = (i:s)
semCmd DUP     vs@(v:_)   = (v:vs)
semCmd ADD     (v1:v2:vs) = (v1+v2:vs)
semCmd MULT    (v1:v2:vs) = (v1*v2:vs)
semCmd INC     (v1:vs)    = (v1+1:vs)
semCmd SWAP    (v1:v2:vs) = (v2:v1:vs)
semCmd (POP i) (_:vs)     = semCmd (POP (i-1)) vs
semCmd _      _           = Nothing

sem :: Prog -> Stack -> Stack
sem []     s = s
sem (p:ps) s = sem ps (semCmd p s)

stackSafe :: Prog -> Bool
stackSafe p = rankP /= Nothing

semStatTC :: Prog -> Maybe Val
semStatTC p | stackSafe p = just sem p
            | otherwise   = Nothing 

--------------------------------------------------------------------
-- ex 2.a                                                         --
--------------------------------------------------------------------

data shape = X
	   | TD Shape Shape
	   | LR Shape Shape

type BBox = (Int,Int)

max :: Int -> Int -> Int
max j k | j > k     = j
	| otherwise = k

bbox :: Shape -> BBox
bbox X 		= (1,1)
bbox (TD s1 s2) = ((max s1x s2x),s1y + s2y)
		   where 
 			(s1x,s1y) = bbox s1
			(s2x,s2y) = bbox s2
bbox (LR s1 s2) = (s1x + s2x, (max s1y s2y))
		   where 
 			(s1x,s1y) = bbox s1
			(s2x,s2y) = bbox s2

--------------------------------------------------------------------
-- ex 2.b                                                         --
--------------------------------------------------------------------

rect :: Shape -> Maybe BBox
rect X	        = Just (1,1)
rect (TD s1 s2) = 
       case (rect s1       , rect s2       ) of
	    (Just (s1x,s1y), Just (s2x,s2y)) -> Just (s1x, s1y+s2y)
            otherwise 		             -> Nothing

--------------------------------------------------------------------
-- ex 3.a                                                         --
--------------------------------------------------------------------
-
-(1) f has the paramter type of x = pointer, y = ??. The return 
-    type is a pointer if x is not null, otherwise it's a list of 
-    y's type. 
-    
-    g has the parameter type of x = pointer, y = ??. The return 
-    type is a list if y's type. 
-
-(2) 
-
-
-
-
-
--------------------------------------------------------------------
-- ex 3.b                                                         --
--------------------------------------------------------------------

h []      _   = []
h _      []   = []
h (b:bs) (ab,abs) | (fst ab)== (fst ab)  = (snd ab):(h bs abs)
		  | otherwise            = b:(h bs abs)



k (a1 b1) ((a1 b2) a3) | a1 == a2  = b1
		       | a1 == a3  = b2
		       | otherwise = b1


















