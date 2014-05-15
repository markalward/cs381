--Homework 3 

--Exercise 1
--Part a

type Prog = [Cmd] 

data Cmd = LD Int 
		| ADD
		| MULT 
		| DUP 
		| INC 
		| SWAP 
		| POP Int 
		deriving Show 

type Rank = Int 
type CmdRank = (Int,Int)
--(Taken from stack, Put on stack) 

--takes a Cmd and gives it a CmdRank
rankC :: Cmd -> CmdRank 
rankC (LD _)  = (0,1)
rankC ADD   = (2,1)
rankC MULT  = (2,1) 
rankC DUP   = (0,1)
rankC INC   = (0,0)
rankC SWAP  = (2,2) 
rankC (POP n) = (n,0) 

--takes a program and outputs Maybe Rank for typesafe
-- and Nothing otherwise 
rankP :: Prog -> Maybe Rank 
rankP []   = Just 0 
rankP cmds = rank cmds 0 

rank :: Prog -> Rank -> Maybe Rank 
rank []         thisRank = Just thisRank
rank (cmd:cmds) startRank | minRank >=0 = rank cmds endRank --does the rest of the program take only the stack that we have 
			  | otherwise   = Nothing 
                          where (pop,push) = rankC cmd 
			        thisRank = push-pop 
				minRank = startRank - pop
			        endRank = startRank + thisRank

--Part b

semStatTC :: Prog -> Maybe Stack 
semStatTC prog | endRank /= Nothing = Just (sem prog []) 
	       | otherwise          = Nothing 
               where endRank = rankP prog 

type Stack = [Int]  

sem :: Prog -> Stack -> Stack 
sem []         myStack = myStack  
sem (cmd:cmds) myStack = sem cmds (semCmd cmd myStack)

semCmd :: Cmd -> Stack -> Stack 
semCmd (LD num ) myStack = num:myStack 
semCmd (ADD)     (x:y:xs) = ((x+y):xs)
semCmd MULT      (x:y:xs) = ((x*y):xs)
semCmd DUP	 (x:xs)   = (x:x:xs)
semCmd INC       (x:xs)   = ((x+1):xs)
semCmd SWAP      (x:y:xs) = (y:x:xs)
semCmd (POP 0)   (xs)     = xs
semCmd (POP n)   (x:xs)   = semCmd (POP (n-1)) xs

exampleProg = [(LD 5),(LD 3),INC,SWAP,DUP,DUP,ADD,MULT,DUP,DUP,DUP,(POP 2)]
badProg = [(LD 3),MULT]

-- Exercise 2 
data Shape = X 
		| TD Shape Shape 
		| LR Shape Shape 
		deriving Show 

--        (width,height)
type BBox = (Int, Int) 

--Part a
bbox :: Shape -> BBox 
bbox X = (1,1)
bbox (TD shape1 shape2) | width2 > width1 =  (width2,height1+height2)--want the max of the two widths, and the combined height
		      | otherwise       =  (width1,height1+height2)
					where (width1,height1) = bbox shape1
					      (width2,height2) = bbox shape2
bbox (LR shape1 shape2) | height2 > height1 =  (width1+width2,height2)--want the max of the two heights, and the combined width
		      | otherwise       =  (width1+width2,height1)
					where (width1,height1) = bbox shape1
					      (width2,height2) = bbox shape2

testBox = (LR(TD(TD X X)(TD X X)) X)
testRect = (LR(TD(TD X X)(TD X X)) (TD(TD X X)(TD X X)))

--Part b
rect :: Shape -> Maybe BBox 
rect X = Just (1,1)
rect (TD shape1 shape2) | width2 == width1 =  Just (width2,height1+height2)--need matching widths, and the combined height
		      | otherwise       =  Nothing
					where (width1,height1) = bbox shape1
					      (width2,height2) = bbox shape2
rect (LR shape1 shape2) | height2 == height1 =  Just (width1+width2,height2)--need matching heights, and the combined width
		      | otherwise       =  Nothing
					where (width1,height1) = bbox shape1
					      (width2,height2) = bbox shape2

--Exercise 3 
--Part a 
{-
	1) f::[a]->a->[a]
	   g::[a]->b->[b]
2) Function f can either output x or [y], infering that both [y] and x must be of the same type.  
Function g can either output [y] or [].  Neither of these include x, so the types of x and y may be separate.  

3) Function g is the more general of the two.  Function g can be called such that it's type is the same as f, but not vice versa.  

4) As explained in 2), f is constrained to have x's elements and y as the same type because of the outputs x and [y].  
Function g doesn't have this constraint; it either outputs [] or [y].  This allows y to be a disjoint type.  
-}
--http://stackoverflow.com/questions/5821089/haskell-function-composition-operator-of-type-cd-abc-abd

--part b 
h :: [b] -> [(a,b)]->[b]
h xs tuple = xs ++ (map snd tuple)

--part c 
k :: (a->b) -> ((a->b)->a) -> b
k forward backward = forward (backward forward)

--pard d 
{-
For a specific set of types, like int's and char's, it would be possible to write this function, 
but it is supposed to be generic.  
There could be a function that maps generic types to ints, but not all types are enumerated (strings
for instance), so that would not work.  If both a and b are enumerated types, we could have 
getMappedB(getIndex(a)), but as a generic function we aren't guaranteed enumerated types.  
The function a->b is a transform function.  Without knowing what the types a and b are 
we cannot make a "kernel" for the transformation.  
-}