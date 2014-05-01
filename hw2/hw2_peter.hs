--------------------------------------------------------------------
-- Peter Rindal, Mark Alward, Brenn Kucey                         --
-- CS381 - HW2: semantics                                         --
-- 4/30/2014                                                      --
--------------------------------------------------------------------
-- ex 1                                                           --
--------------------------------------------------------------------





--------------------------------------------------------------------
-- ex 2.a                                                         --
--------------------------------------------------------------------




--------------------------------------------------------------------
-- ex 3  														  --
--------------------------------------------------------------------

data Cmd = Pen Mode
         | MoveTo Int Int
         | Seq Cmd Cmd

data Mode = Up | Down

type State = (Mode,Int,Int)
type Line = (Int,Int,Int,Int)
type Lines = [Line]

semS :: Cmd -> State -> (State,Lines)
semS (Pen    m    ) (_,   x,y) = ((m   ,x ,y ), [           ]        )
semS (MoveTo x' y') (Down,x,y) = ((Down,x',y'), [(x,y,x',y')]        )
semS (MoveTo x' y') (Up,  _,_) = ((Up  ,x',y'), [           ]        )
semS (Seq    s1 s2)  st        = ( fst r2     , (snd r1) ++ (snd r2) )
                    where 
                        r1 = semS s1 st
                        r2 = semS s2 (fst r1)

sem' :: Cmd -> Lines
sem' c = snd (semS c (Up,0,0)) 


semTest = (Seq (Pen Down)
          (Seq (MoveTo 3 3)
          (Seq (MoveTo 1 0)
          (Seq (Pen Up)
          (Seq (MoveTo 4 5)
          (Seq (Pen Down)
               (MoveTo 3 2)
          ) ) ) ) ) )