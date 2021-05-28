// Gianni Monteban & Martijn Vogelaar
// 1047546 & 1047391
module clean7


import StdEnv, StdMaybe, StdList


:: High = High
:: Low = Low
:: Full = Full
:: Free = Free
:: ErrorOrResult e r = Error e | Result r

:: S a :== ErrorOrResult String State


moveToQuay :: (S (High,a)) -> (S (High,a))
moveToQuay (Result st) = Result ({st & craneOnQuay=True})
moveToQuay e = e

moveToShip :: (S (High,a)) -> (S (High,a))
moveToShip (Result st) = Result ({st & craneOnQuay = False})
moveToShip e = e

moveUp :: (S (Low, a)) -> (S (High,a))
moveUp (Result st) = Result ({st & craneUp = True})
moveUp e = e

moveDown :: (S (High, a)) -> (S (Low, a))
moveDown (Result st) = Result ({st & craneUp = False})
moveDown e = e

lock :: (S (Low,Free)) -> (S (Low, Full))
lock (Result st) = if (st.craneOnQuay) (case st.onQuay of
                                    [] = Error "No containers on quay"
                                    [x:xs] = Result {st & onQuay = xs, locked = Just x}) 
                                (case st.onShip of
                                    [] = Error "No containers on ship"
                                    [x:xs] = Result {st & onShip = xs, locked = Just x})
lock e = e

unlock :: (S (Low,Full)) -> (S (Low, Free))
unlock (Result st) = if (st.craneOnQuay) (Result {st & onQuay = [(fromJust st.locked) :st.onQuay], locked = Nothing}) 
                                (Result {st & onShip = [(fromJust st.locked) :st.onShip], locked = Nothing})
unlock e = e

wait :: (S (a, b)) -> (S (a, b))
wait a = a


whileContainerBelow :: (S a) ((S a) -> (S a)) -> (S a)
whileContainerBelow (Result st) f = if (currentPos st) (whileContainerBelow (f (Result st)) f) (Result st)
                        where currentPos state = (if (state.craneOnQuay) ( not(isEmpty state.onQuay)) (not (isEmpty state.onShip)))   
whileContainerBelow e _ = e              

(:.) infixl 1 ::  ((S a) -> (S b)) ((S b) -> (S c)) -> ((S a) -> (S c)) 
(:.) f g = g o f


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


init :: S (High,Free) // high needs to match craneUp and Free with locked = Nothing
init = Result initialState



loadShip = whileContainerBelow init (moveDown :. lock :. moveUp :. moveToShip :. wait :. moveDown :. wait :. unlock :. moveUp :. moveToQuay )


Start = loadShip

