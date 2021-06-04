// Gianni Monteban & Martijn Vogelaar
// 1047546 & 1047391
module clean8


import StdEnv, StdMaybe, StdList

:: Show a = Print [String]

class Action v where
    MoveToShip :: v (Step High High)                                      // move the crane to the ship
    MoveToQuay :: v (Step High High)                                    // move the crane to the quay
    MoveUp     :: v (Step Low High)                                         // moves the crane up
    MoveDown   :: v (Step High Low)                                          // moves the crane down
    Lock       :: v (Step Low Low)                                        // locks the top container of the stack under the crane
    Unlock     :: v (Step Low Low)                                   // unlocks the container the crane is carrying, put it on the stack
    Wait       :: v (Step a a)                                      // do nothing
    (:.) infixl 1 :: (v (Step a b)) (v (Step b c)) -> (v (Step a c))                  // sequence of two actions
    While :: (v Bool) (v (Step a a)) -> (v (Step a a))                       // repeat action while there is a container at current position

instance Action Show where
    MoveToShip = Print ["MoveToShip"]
    MoveToQuay = Print ["MoveToQuay"]
    MoveDown = Print ["MoveDown"]
    MoveUp = Print ["MoveUp"]
    Lock = Print ["Lock"]
    Unlock = Print ["Unlock"]
    Wait = Print ["Wait"]
    (:.) (Print a) (Print b) = Print (a ++ [":."] ++ b)
    While (Print e) (Print a) = Print (["While"] ++ ["("] ++ e ++ [")"] ++ ["("] ++ a ++ [")"])

instance Action UseState where
    MoveToShip = UseState \s -> Result (Step {s & craneOnQuay = False})
    MoveToQuay = UseState \s -> Result (Step {s & craneOnQuay = True})
    MoveDown = UseState \s -> Result (Step {s & craneUp = False})
    MoveUp = UseState \s -> Result (Step {s & craneUp = True})
    Lock = UseState \s -> if (s.craneOnQuay) (case s.onQuay of
                                    [] = Error "No containers on quay"
                                    [x:xs] = Result (Step {s & onQuay = xs, locked = Just x})) 
                                (case s.onShip of
                                    [] = Error "No containers on ship"
                                    [x:xs] = Result (Step {s & onShip = xs, locked = Just x}))

    Unlock = UseState \s -> if (s.craneOnQuay) (Result (Step {s & onQuay = [(fromJust s.locked) :s.onQuay], locked = Nothing})) 
                                (Result (Step {s & onShip = [(fromJust s.locked) :s.onShip], locked = Nothing}))
    Wait = UseState \s -> Result (Step s)
    (:.) (UseState a) (UseState b) = UseState \s ->  case a s of
                                                    Result (Step x) = b x
                                                    _ = Error "Error during execution"
                                                

    While (UseState e) (UseState a) = UseState \s -> Result (Step s)
        



class Expr x where
    ContainersBelow :: (x Int) // number of containers at current position
    Lit :: t -> x t | toString t
    (<.) infix 4 :: (x t) (x t) -> x Bool | <, toString t
    (>.) infix 4 :: (x t) (x t) -> x Bool | <, toString t
    (+.) infix 4 :: (x Int) (x Int) -> x Int

instance Expr Show where
    ContainersBelow = Print ["ContainersBelow"]
    Lit t = Print (["Lit"] ++ [toString t])
    (<.) (Print a) (Print b) = Print (a ++ ["<."]  ++ b)
    (>.) (Print a) (Print b) = Print (a ++ [">."] ++ b)
    (+.) (Print a) (Print b) = Print (a ++ ["+."] ++ b)

instance Expr UseState where
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

:: Step init target = Step State

:: ErrorOrResultOrExpr e r = Error e | Result r

:: UseState a = UseState (State -> ErrorOrResultOrExpr String a )


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
Start = print loadShip

print (Print l) = unlines l


unlines :: [String] -> String
unlines xs = foldr (\x acc -> x +++ acc) "" xs