module genericMap

/*
  Genric map definition for assignment 8 in AFP 20-21
  Pieter Koopman, pieter@cs.ru.nl
  
  Use StdEnv or iTask environment.
*/

import StdEnv, StdGeneric


:: Bin a = Leaf | Bin (Bin a) a (Bin a)
t = Bin (Bin Leaf 1 Leaf) 2 (Bin (Bin Leaf 3 Leaf) 4 Leaf)

:: Pos a = {x :: a, y :: a}
p = {x = 2.3, y = 4.5}

generic gMap a b :: a -> b

gMap{|c|}        x = x
gMap{|UNIT|}     x = x
gMap{|PAIR|}   f g (PAIR x y) = PAIR   (f x) (g y) 
gMap{|EITHER|} f g (LEFT x)   = LEFT   (f x)
gMap{|EITHER|} f g (RIGHT x)  = RIGHT  (g x)
gMap{|CONS|}   f   (CONS x)   = CONS   (f x)
gMap{|OBJECT|} f   (OBJECT x) = OBJECT (f x)
gMap{|RECORD|} f   (RECORD x) = RECORD (f x)
gMap{|FIELD|}  f   (FIELD  x) = FIELD  (f x)
derive gMap Pos,  Bin, []


generic gEq a :: a a -> Bool
gEq{|UNIT|} _ _ = True
gEq{|PAIR|} f g (PAIR a b) (PAIR x y) = f a x && g b y
gEq{|EITHER|} f g (LEFT x) (LEFT y)= f x y
gEq{|EITHER|} f g (RIGHT x)  (RIGHT y)  = g x y
gEq{|EITHER|} f g  _         _          = False
gEq{|CONS|}   f   (CONS a)   (CONS b)   = f a b
gEq{|OBJECT|} f   (OBJECT a) (OBJECT b) = f a b
gEq{|RECORD|} f   (RECORD a) (RECORD b) = f a b
gEq{|FIELD|}  f   (FIELD a)  (FIELD b)  = f a b
gEq{|Int|}            a b  =            a == b
// gEq{|Int|}         a b  =            a < b

derive gEq []


swap (a) = {x = a.y, y = a.x}

l = [0..7]

fac :: Int -> Int
fac 0 = 1
fac i = i * fac (i-1)

Start = (a,b,c,d,e,f,g)

a = gMap{|*->*|} fac t
b = gMap{|*->*|} (\x -> (x,fac x)) l
c = (\(x,y) -> (gMap{|*->*|} fac x, gMap{|*->*|} fac y)) (l,t)
d = gMap{|*->*|} toInt p
e = gMap{|*->*|} swap [p:[p]]

f = gEq{|*|} [1,2] [1,2]
g = gEq{|*|} [1,2] [2,3]
// h, changing the == to < results in f being false and g being true.