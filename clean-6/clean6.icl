// Gianni Monteban & Martijn Vogelaar
// 1047546 & 1047391
module clean6


import StdEnv, StdMaybe, StdList

:: BM a b = {ab::a->b,ba::b->a}
bm :: BM a a
bm = {ab=id, ba=id}

:: Action a b
    = MoveToShip (BM a High) (BM b High)                                        // move the crane to the ship
    | MoveToQuay (BM a High) (BM b High)                                       // move the crane to the quay
    | MoveUp     (BM a Low) (BM b High)                                           // moves the crane up
    | MoveDown   (BM a High) (BM b Low)                                          // moves the crane down
    | Lock       (BM a Low) (BM b Low)                                        // locks the top container of the stack under the crane
    | Unlock     (BM a Low) (BM b Low)                                       // unlocks the container the crane is carrying, put it on the stack
    | Wait       (BM a b)                                       // do nothing
    | E.c: (:.) infixl 1 (Action a c) (Action c b)                     // sequence of two actions
    | WhileContainerBelow (Action a b)                        // repeat action while there is a container at current position


:: High = High
:: Low = Low

:: ErrorOrResult e r = Error e | Result r

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





moveToShip = MoveToShip bm bm
moveToQuay = MoveToQuay bm bm
moveUp = MoveUp bm bm
moveDown = MoveDown bm bm
lock = Lock bm bm
unlock = Unlock bm bm
wait = Wait bm

//
loadShip = WhileContainerBelow (moveDown :. lock :. moveUp :. moveToShip :. wait :. moveDown :. wait :. unlock :. moveUp :. moveToQuay )



eval :: (Action a b) State -> ErrorOrResult String State
eval a st = case a of
        MoveToShip _ _ = Result ({st & craneOnQuay = False})
        MoveToQuay _ _ = Result ({st & craneOnQuay=True})
        MoveUp _ _ = Result ({st & craneUp = True})
        MoveDown _ _= Result ({st & craneUp = False})
        Lock _ _= case st.locked of
                    Just c = Error "We already locked something"
                    Nothing = if (st.craneOnQuay) (case st.onQuay of
                                                        [] = Error "No containers on quay"
                                                        [x:xs] = Result {st & onQuay = xs, locked = Just x}) 
                                (case st.onShip of
                                    [] = Error "No containers on ship"
                                    [x:xs] = Result {st & onShip = xs, locked = Just x})
        Unlock _ _ = case st.locked of
                    Nothing = Error "We have nothing to unlock"
                    Just c = if (st.craneOnQuay) (Result {st & onQuay = [c:st.onQuay], locked = Nothing}) 
                                (Result {st & onShip = [c:st.onShip], locked = Nothing})
        Wait _ = Result st
        (:. a1 a2) = case eval a1 st of
                        Error e = Error e
                        Result st1 = eval a2 st1
        WhileContainerBelow x = if (currentPos st) (case eval x st of
                                                    Error a = Error a
                                                    Result st1 = eval (WhileContainerBelow x) st1) (Result st) 
                            where currentPos state = (if (state.craneOnQuay) ( not(isEmpty state.onQuay)) (not (isEmpty state.onShip)))  

print :: (Action a b) -> [String]
print a = case a of
        MoveToShip _ _ = ["Move to Ship"]
        MoveToQuay _ _ = ["Move to Quay"]
        MoveUp _ _ = ["Move up"]
        MoveDown _ _= ["Move down"]
        Lock _ _= ["Lock"]
        Unlock _ _ =["UnLock"]
        Wait _ = ["Wait"]
        (:. a1 a2) = print a1 ++ print a2
        WhileContainerBelow x = ["While container below": print x]



// Start = eval loadShip initialState 
Start = print loadShip

