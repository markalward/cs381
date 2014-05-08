
--------------------------------------------------------------------
-- ex 1.a                                                         --
--------------------------------------------------------------------

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
rankC (LD _)    = (0, 1)
rankC ADD       = (2, 1)
rankC MULT      = (2, 1)
rankC DUP       = (1, 2)
rankC INC       = (1, 1)
rankC SWAP      = (2, 2)
rankC (POP _)   = (1, 0)


rank :: Prog -> Rank -> Maybe Rank
rank [] i       = Just i
rank (x:xs) i   | (i < pop) = Nothing
                | otherwise = rank xs (i - pop + push)
                where (pop, push) = rankC x


rankP :: Prog -> Maybe Rank
rankP xs        = rank xs 0

--------------------------------------------------------------------
-- ex 1.b                                                         --
--------------------------------------------------------------------

type Stack = [Int]

-- sem does not need to be able to handle error domains since sem 
-- will only be called after static type checking
sem :: Prog -> Stack -> Stack
sem _ _ = []

typeSafe :: Prog -> Bool
typeSafe p = rankP p /= Nothing

semStatTC :: Prog -> Maybe Stack
semStatTC p     | typeSafe p = Just (sem p [])
                | otherwise  = Nothing

--------------------------------------------------------------------
-- ex 2.a                                                         --
--------------------------------------------------------------------

data Shape = X
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int,Int)

bbox :: Shape -> BBox
bbox X          = (1,1)
bbox (TD a b)   = (max ax bx, ay + by)
                where   (ax, ay) = bbox a
                        (bx, by) = bbox b
bbox (LR a b)   = (ax + bx, max ay by)
                where   (ax, ay) = bbox a
                        (bx, by) = bbox b
   

--------------------------------------------------------------------
-- ex 2.b                                                         --
--------------------------------------------------------------------

-- ?

--------------------------------------------------------------------
-- ex 3.a                                                         --
--------------------------------------------------------------------
-- (a) Consider the functions f and g, which are given by the following two function definitions.
f x y = if null x then [y] else x
g x y = if not (null x) then [] else [y]
-- (1) What are the types of f and g?
--      f :: [a] -> a -> [a]
--      g :: [a] -> b -> [b]
-- (2) Explain why the functions have these types.
--      f:  "null x" implies that x has type [a]. Then the else expression has type [a].
--          The then expression has to have the same type as the else expression, so
--          y has type a.
--      g:  "null x" implies that x has type [a]. If y has type b, then the else expression
--          has type [b]. The then expression will have type [b] as well.
-- (3) Which type is more general?
--      
-- (4) Why do f and g have different types?


--------------------------------------------------------------------
-- ex 3.b                                                         --
--------------------------------------------------------------------
-- h :: [b] -> [(a, b)] -> [b]
h x y = if True then map snd y else x

--------------------------------------------------------------------
-- ex 3.c                                                         --
--------------------------------------------------------------------
-- k :: (a -> b) -> ((a -> b) -> a) -> b
k x y = x (y x)





