module serialize7start

/*
  Definition for assignment 7 in AFP 2021
  Pieter Koopman pieter@cs.ru.nl

  Use this in a project with Environment StdEnv
  Use project option 'Basic Values Only' for nicer output
*/


/*
Gianni Monteban & Martijn Vogelaar
1047546 & 104391

1.1 Since UNIT can't be anything else then UNIT, option 2 definitly isn't the best solution. Option 1 can be a bit odd, since anything is accepted.
But anything is in this case only UNIT. We prefer option 3, since it is very clear and it works the same as the other solutions.

1.2 The constructor name is just there to do printing, it is not necessary for the equal.

1.3
[] -> Nil -> (CONS UNIT)
Leaf -> (CONS UNIT)

No the typesystem wil break since we can't compare two things which aren't of the same datatype.


3 Reflection

It goes wrong if we take a 3 dimensional array
test [[[[2]]]]
This will break the serialize
*/

import StdEnv, StdMaybe

class serialize a where
  write :: a [String] -> [String]
  read  :: [String] -> Maybe (a,[String])

instance serialize Bool where
  write b c = [toString b:c]
  read ["True":r]  = Just (True,r)
  read ["False":r] = Just (False,r)
  read _ = Nothing

instance serialize Int where
  write i c = [toString i:c]
  read [s:r]
    # i = toInt s
    | s == toString i
      = Just (i,r)
      = Nothing
  read _ = Nothing

// ---

:: UNIT       = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR   a b = PAIR a b
:: CONS   a   = CONS String a

// ---

instance serialize UNIT where
  write _ c = c
  read [] = Nothing
  read r = Just (UNIT,r)

instance serialize (EITHER a b) | serialize a & serialize b where
  write (RIGHT b) l = write b l
  write (LEFT a) l = write a l
  read [] = Nothing
  read r = case read r of
            Just(x,y) = Just (RIGHT x , y)
            Nothing = case read r of
                        Just (a,b) = Just (LEFT a, b)
                        Nothing = Nothing




instance serialize (PAIR a b) | serialize a & serialize b where
  write (PAIR a b) l = let ll = write b l in  write a ll
  read [] = Nothing
  read r = case read r of
                      Just (y,z) = case read z of
                                    Just (x,s) = Just (PAIR y x, s)
                                    Nothing = Nothing
                      Nothing = Nothing

instance serialize (CONS a) | serialize a where
  write (CONS s a) l = [s : write a l]
  read [] = Nothing
  read [r:s] = case read s of
                  Just (y,z) = Just(CONS r y, z)
                  Nothing = Nothing

:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))

fromList :: [a] -> ListG a
fromList [] = LEFT (CONS "Nil" UNIT)
fromList [x:xs] = RIGHT (CONS "Cons" (PAIR x xs))

toList   ::(ListG a) -> Maybe [a]
toList (LEFT (CONS "Nil" UNIT)) = Just []
toList (RIGHT (CONS "Cons" (PAIR x xs))) = Just [x:xs]
toList _ = Nothing

instance serialize [a] | serialize a where  // to be improved
  write l c =  write (fromList l) c
  read [] = Nothing
  read l = case read l of
            Just (x,y) = case toList x of
                        Just z = Just (z,y)
                        Nothing = Nothing
            Nothing = Nothing

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

fromBin ::(Bin a) -> BinG a
fromBin Leaf = LEFT (CONS "Leaf" UNIT)
fromBin (Bin a b c) = RIGHT (CONS "Bin" (PAIR a (PAIR b c)))

toBin ::(BinG a) -> Maybe (Bin a)
toBin (LEFT (CONS "Leaf" UNIT)) = Just Leaf
toBin (RIGHT (CONS "Bin" (PAIR a (PAIR b c)))) = Just( Bin a b c)
toBin _ = Nothing

instance serialize (Bin a) | serialize a where // to be improved
  write a c = write (fromBin a) c
  read [] = Nothing
  read l = case read l of
            Just (x,y) = case toBin x of
                        Just a = Just (a,y)
                        Nothing = Nothing
            Nothing = Nothing

instance == (Bin a) | == a where // better use the generic approach
  (==) Leaf Leaf = True
  (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
  (==) _ _ = False

// ---

Start =
  [test True
  ,test False
  ,test 0
  ,test 123
  ,test -36
  ,test [42]
  ,test [0..4]
  ,test [[True],[]]
  ,test (Bin Leaf True Leaf)
  ,test (Bin Leaf 2 Leaf)
  ,test (Bin Leaf [2] Leaf)
  ,test [Bin Leaf [2] Leaf]
  ,test [Bin Leaf 2 Leaf]
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin Leaf [4,5] Leaf))]
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin (Bin Leaf [4,5] Leaf) [6,7] (Bin Leaf [8,9] Leaf)))]
  , test [[[[2]]]]
  ]

test :: a -> ([String],[String]) | serialize, == a
test a =
  (if (isJust r)
    (if (fst jr == a)
      (if (isEmpty (tl (snd jr)))
        ["Oke "]
        ["Fail: not all input is consumed! ":snd jr])
      ["Fail: Wrong result ":write (fst jr) []])
    ["Fail: read result is Nothing "]
  , ["write produces ": s]
  )
  where
    s = write a ["\n"]
    r = read s
    jr = fromJust r
