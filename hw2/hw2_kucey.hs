--Exercise 1 **********************************************

type Prog = [Cmd]

data Cmd = LD Int 
		 | ADD 
		 | MULT 
		 | DUP 
		 deriving Show
		 
type Stack = Maybe [Int] 

type D = Stack -> Stack

semCmd :: Cmd -> D 

semCmd (LD x) (Just xs) = Just (x:xs)
semCmd (LD x) Nothing   = Nothing 

semCmd ADD (Just [])       = Nothing
--semCmd ADD (Just x:[y])     = Nothing
semCmd ADD (Just (x:y:xs)) = Just ((x+y):xs)
semCmd ADD Nothing         = Nothing

semCmd MULT (Just [])   = Nothing
--semCmd MULT (Just (x:[y]))  = Nothing
semCmd MULT (Just (x:y:xs)) = Just ((x*y):xs)
semCmd MULT Nothing         = Nothing

semCmd DUP (Just [])      = Nothing
semCmd DUP  (Just (x:xs)) = Just (x:x:xs)
semCmd DUP Nothing        = Nothing

myStack = Just [4,5,6,7]
myAdd = semCmd ADD (semCmd ADD myStack) 
myMult = semCmd MULT myStack 
myDup = semCmd DUP myStack 
myLd = semCmd (LD 3) myStack 

--sem :: Prog -> D 

--Example Programs ******
p1 :: Prog
p1 = [LD 3, DUP, ADD, DUP,MULT] 
p2 :: Prog
p2 = [LD 3, ADD]
p3 :: Prog
p3 = []