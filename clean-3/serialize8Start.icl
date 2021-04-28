module serialize8Start

/*
  Definitions for assignment 8 in AFP 20-21
  Kind indexed gennerics
  Pieter Koopman, pieter@cs.ru.nl
  
  Use StdEnv or iTask environment.
  Use Basic Values Only as conclose option for a nicer output.
*/

import StdEnv, StdMaybe, StdGeneric

// use this as serialize0 for kind *
class serialize a | read {|*|}, write {|*|} a

// ---
generic write a :: a [String] -> [String]
write{|Int|} a s = [toString a:s]
write{|Bool|} a s = [toString a:s]
write{|UNIT|} a s = s
write{|EITHER|} f g (LEFT x) s = f x s
write{|EITHER|} f g (RIGHT x) s = g x s
write{|PAIR|} f g (PAIR x y) s = let z = g y s in f x z
write{|CONS of c|} f (CONS x) r = ["(" : c.gcd_name : f x [")" : r]]
write{|OBJECT|} f   (OBJECT x) s = f x s
write{|RECORD|} f   (RECORD x) s =f x s
write{|FIELD|}  f   (FIELD x) s =f x s
write{|(,)|} f g (x,y) s = let z = [ "," : g y [")":s]] in ["(" : f x z]
derive write Coin, [], Bin

generic read a :: [String] -> Maybe(a,[String])
read{|Int|} [s:r]
    # i = toInt s
    | s == toString i
      = Just (i,r)
      = Nothing
read{|Int|} _ = Nothing
read{|Bool|} ["True":r]  = Just (True,r)
read{|Bool|} ["False":r] = Just (False,r)
read{|Bool|} _ = Nothing
read{|UNIT|} [] = Nothing
read{|UNIT|} s = Just(UNIT,s)
read{|EITHER|} _ _ [] = Nothing
read{|EITHER|} f g r = case f r of
                        Just(x,y) = Just (LEFT (x) , y)
                        Nothing = case g r of
                                    Just (a,b) = Just (RIGHT (a), b)
                                    Nothing = Nothing
read{|PAIR|} _ _ [] = Nothing
read{|PAIR|} f g s = case f s of
                        Just (y,z) = case g z of
                                      Just (x,r) = Just (PAIR (y) (x), r)
                                      Nothing = Nothing
                        Nothing = Nothing
read{|CONS of c|} f ["(" : name : r] 
    | name == c.gcd_name = case f r of
        Just (x, [")" : z]) = Just (CONS x, z)
        _                    = Nothing
    | otherwise = Nothing
read{|CONS|} _ _ = Nothing
read{|OBJECT|}  _  [] = Nothing
read{|OBJECT|} f  s = case f s of
                        Just (y,z) = Just(OBJECT (y), z)
                        Nothing = Nothing
read{|RECORD|}  _  [] = Nothing
read{|RECORD|} f s = case f s of
                        Just (y,z) = Just(RECORD ( y), z)
                        Nothing = Nothing
read{|FIELD|}  _  [] = Nothing
read{|FIELD|}  f  s = case f s of
                        Just (y,z) = Just(FIELD (y), z)
                        Nothing = Nothing
read{|(,)|} f g ["(":xs] = case f xs of
                    Just (y,[",":ys]) = case g ys of
                                          Just(z,[")":c]) = Just ((y,z),c)
                                          Nothing = Nothing
                    Nothing = Nothing

derive read Coin, [], Bin
// ---

:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))

fromList :: [a] -> ListG a
fromList []    = LEFT  (CONS  UNIT)
fromList [a:x] = RIGHT (CONS (PAIR a x))

toList :: (ListG a) -> [a]
toList (LEFT  (CONS UNIT))       = []
toList (RIGHT (CONS (PAIR a x))) = [a:x]

NilString  :== "Nil"
ConsString :== "Cons"



:: Bin a = Leaf | Bin (Bin a) a (Bin a)

:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

fromBin :: (Bin a) -> BinG a
fromBin Leaf = LEFT (CONS UNIT)
fromBin (Bin l a r) = RIGHT (CONS (PAIR l (PAIR a r)))

toBin :: (BinG a) -> Bin a
toBin (LEFT (CONS UNIT)) = Leaf
toBin (RIGHT (CONS (PAIR l (PAIR a r)))) = Bin l a r

LeafString :== "Leaf"
BinString  :== "Bin"

instance == (Bin a) | == a where
  (==) Leaf Leaf = True
  (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
  (==) _ _ = False


:: Coin = Head | Tail
:: CoinG :== EITHER (CONS UNIT) (CONS UNIT)

fromCoin :: Coin -> CoinG
fromCoin Head = LEFT (CONS UNIT)
fromCoin Tail = RIGHT (CONS UNIT)

toCoin :: CoinG -> Coin
toCoin (LEFT (CONS UNIT)) = Head
toCoin (RIGHT (CONS UNIT)) = Tail

instance == Coin where
  (==) Head Head = True
  (==) Tail Tail = True
  (==) _    _    = False

// output looks nice if compiled with "Basic Values Only" for console in project options
Start = 
  [test True
  ,test False
  ,test 0
  ,test 123
  ,test -36
  ,test [42]
  ,test [0..4]
  ,test [True]
  ,test [[1]]
  ,test [[True],[]]
  ,test [[[1]],[[2],[3,4]],[[]]]
  ,test (Bin Leaf True Leaf)
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin Leaf [4,5] Leaf))]
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin (Bin Leaf [4,5] Leaf) [6,7] (Bin Leaf [8,9] Leaf)))]
  ,test Head
  ,test Tail
  ,test (7,True)
  ,test (Head,(7,[Tail]))
  ,["End of the tests.\n"]
  ]

test :: a -> [String] | serialize, == a
test a = 
  (if (isJust r)
    (if (fst jr == a)
      (if (isEmpty (tl (snd jr)))
        ["Oke"]
        ["Not all input is consumed! ":snd jr])
      ["Wrong result: ":write{|*|} (fst jr) []])
    ["read result is Nothing"]
  ) ++ [", write produces: ": s]
  where
    s = write{|*|} a ["\n"]
    r = read{|*|} s
    jr = fromJust r
