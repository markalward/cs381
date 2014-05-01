
-- ex 1

-- this is also 2a
type Prog = [Cmd]
data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | DEF String Prog
         | CALL String
         deriving Show

type Stack = [Int]

-- D is the semantic domain
type D = Maybe Stack -> Maybe Stack

semCmd :: Cmd -> D
semCmd _ Nothing = Nothing
semCmd (LD i) (Just st) = Just (i : st)
semCmd ADD (Just st) = case st of (x:y:xs) -> Just ((x+y) : xs)
                                  _ -> Nothing
semCmd MULT (Just st) = case st of (x:y:xs) -> Just ((x*y) : xs)
                                   _ -> Nothing
semCmd DUP (Just st) = case st of (x:xs) -> Just (x:x:xs)
                                  _ -> Nothing

sem :: Prog -> D
sem _ Nothing = Nothing
sem [] st = st
sem (x:xs) st = case semCmd x st of Nothing -> Nothing
                                    newState -> sem xs newState

-- ex 2

-- 2b)

type Macros = [(String,Prog)]

data State = S Stack Macros | Error deriving Show

-- 2c)

semCmd2 :: Cmd -> State -> State
semCmd2 _ Error = Error
semCmd2 (DEF name p) (S st macros) = S st ((name, p) : macros)
semCmd2 (CALL name) s@(S st macros) = case lookup name macros of
                                        Nothing -> Error
                                        Just p -> sem2 p s
semCmd2 (LD i) (S st macros) = S (i : st) macros
semCmd2 ADD (S st macros) = case st of (x:y:xs) -> S ((x+y) : xs) macros
                                       _ -> Error
semCmd2 MULT (S st macros) = case st of (x:y:xs) -> S ((x*y) : xs) macros
                                        _ -> Error
semCmd2 DUP (S st macros) = case st of (x:xs) -> S (x:x:xs) macros
                                       _ -> Error


sem2 :: Prog -> State -> State
sem2 _ Error = Error
sem2 [] st = st
sem2 (x:xs) st = case semCmd2 x st of Error -> Error
                                      newState -> sem2 xs newState


-- ex 3

data Cmd = Pen Mode
         | MoveTo Int Int
         | Seq Cmd Cmd
         deriving Show
data Mode = Up | Down
            deriving Show

type State = (Mode,Int,Int)
type Line = (Int,Int,Int,Int)
type Lines = [Line]

semS :: Cmd -> State -> (State,Lines)
semS (Pen d) (_, x, y) = ((d, x, y), [])
semS (MoveTo i j) (Down, x, y) = ((Down, i, j), [(x,y,i,j)])
semS (MoveTo i j) (Up, _, _) = ((Up, i, j), [])
semS (Seq a b) state = (finalState, linesA ++ linesB)
                    where (newState, linesA) = semS a state
                          (finalState, linesB) = semS b newState

sem' :: Cmd -> Lines
sem' c = snd (semS c (Up, 0, 0))




