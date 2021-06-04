// Gianni Monteban & Martijn Vogelaar
// 1047546 & 1047391
module clean8


import StdEnv, StdMaybe, StdList

:: Show a b c = Print [String]

class Action v where
    MoveToShip :: v (High,a) (High, a) State                                   // move the crane to the ship
    MoveToQuay :: v (High,a) (High, a) State                                    // move the crane to the quay
    MoveUp     :: v (Low,a) (High, a) State                                         // moves the crane up
    MoveDown   :: v (High,a) (Low, a) State                                         // moves the crane down
    Lock       :: v (Low,Free) (Low, Full) State                                   // locks the top container of the stack under the crane
    Unlock     :: v (Low,Full) (Low, Free) State                                  // unlocks the container the crane is carrying, put it on the stack
    Wait       :: v a a State                                    // do nothing
    (:.) infixl 1 :: (v a b State) (v b d State) -> (v a d State)                  // sequence of two actions
    While :: (v a a Bool) -> (v a a State) -> (v a a State)                       // repeat action while there is a container at current position

instance Action Show where
    MoveToShip = Print ["MoveToShip"]
    MoveToQuay = Print ["MoveToQuay"]
    MoveDown = Print ["MoveDown"]
    MoveUp = Print ["MoveUp"]
    Lock = Print ["Lock"]
    Unlock = Print ["Unlock"]
    Wait = Print ["Wait"]
    (:.) (Print a) (Print b) = Print (a ++ [":."] ++ b)
    While (Print e) = \(Print a) ->  Print (["While"] ++ ["("] ++ e ++ [")"] ++ ["("] ++ a ++ [")"])

instance Action UseState where
    MoveToShip = UseState \s -> Result {s & craneOnQuay = False}
    MoveToQuay = UseState \s -> Result {s & craneOnQuay = True}
    MoveDown = UseState \s -> Result {s & craneUp = False}
    MoveUp = UseState \s -> Result {s & craneUp = True}
    Lock = UseState \s -> if (s.craneOnQuay) (case s.onQuay of
                                    [] = Error "No containers on quay"
                                    [x:xs] = Result {s & onQuay = xs, locked = Just x})
                                (case s.onShip of
                                    [] = Error "No containers on ship"
                                    [x:xs] = Result  {s & onShip = xs, locked = Just x})

    Unlock = UseState \s -> if (s.craneOnQuay) (Result  {s & onQuay = [(fromJust s.locked) :s.onQuay], locked = Nothing})
                                (Result  {s & onShip = [(fromJust s.locked) :s.onShip], locked = Nothing})
    Wait = UseState \s -> Result  s
    (:.) (UseState a) (UseState b) = UseState \s ->  case a s of
                                                    Result  x = b x
                                                    _ = Error "Error during execution"
                                                

    While (UseState e) = \a -> UseState \s ->  if (fromResult (e s)) ( case (fromUsage a) s of
                                    Error e = Error e
                                    Result x = Result x) (Result s)
                                    where fromResult (Result b) = b
                                          fromUsage (UseState a ) =a



class Expr x where
    ContainersBelow :: (x Int) // number of containers at current position
    Lit :: t -> x t | toString t
    (<.) infix 4 :: (x t) (x t) -> x Bool | <, toString t
    (>.) infix 4 :: (x t) (x t) -> x Bool | <, toString t
    (+.) infix 4 :: (x Int) (x Int) -> x Int

instance Expr (Show a b) where
    ContainersBelow = Print ["ContainersBelow"]
    Lit t = Print (["Lit"] ++ [toString t])
    (<.) (Print a) (Print b) = Print (a ++ ["<."]  ++ b)
    (>.) (Print a) (Print b) = Print (a ++ [">."] ++ b)
    (+.) (Print a) (Print b) = Print (a ++ ["+."] ++ b)

instance Expr (UseState a b) where
    ContainersBelow = UseState \s -> Result if (s.craneOnQuay) (length s.onQuay) (length s.onShip)
    Lit t = UseState \s -> Result t
    (<.) (UseState a) (UseState b) = UseState \s -> case a s of 
                                                        Result x = case b s of
                                                                        Result y = Result (x < y)
    (>.) (UseState a) (UseState b) = UseState \s -> case a s of 
                                                        Result x = case b s of
                                                                        Result y = Result (x > y)
    (+.) (UseState a) (UseState b) = UseState \s -> case a s of 
                                                        Result x = case b s of
                                                                        Result y = Result (x + y)

:: High = High
:: Low = Low
:: Full = Full
:: Free = Free


:: ErrorOrResultOrExpr e r = Error e | Result r

:: UseState a b c  = UseState (State -> ErrorOrResultOrExpr String c )


:: State =
    { onShip :: [Container]
    , onQuay :: [Container]
    , craneUp :: Bool
    , craneOnQuay :: Bool
    , locked :: Maybe Container
    }
    
:: Container :== String

initialState = {
            onShip = []
            , onQuay = ["apples","beer","camera’s"]
            , craneUp = True
            , craneOnQuay = True
            , locked = Nothing
            }




loadShip = While (ContainersBelow >. Lit 0) (
    MoveDown :.
    Lock:.
    MoveUp:.
    MoveToShip:.
    Wait:.
    MoveDown:.
    Wait:.
    Unlock:.
    MoveUp:.
    MoveToQuay)




// Start = eval loadShip initialState 
Start = eval loadShip

eval (UseState x) = x initialState

print (Print l) = unlines l


unlines :: [String] -> String
unlines xs = foldr (\x acc -> x +++ acc) "" xs