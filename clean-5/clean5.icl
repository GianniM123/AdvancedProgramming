module clean5

import iTasks

:: Login =
    { userName :: String,
      pass :: Password
    }

instance == Login where
    (==) l1 l2 = (l1.userName == l2.userName) && (l1.pass == l2.pass) 

:: ToDo =
    { todo :: String
    , limit :: Date
    , names :: [ String ]
    }

derive class iTask Login, ToDo

toDoTaks = sharedStore "ToDo" []

// loginInfo :: Shared [Login]
loginInfo = sharedStore "myLogins" [{userName="Jan",pass=Password "1234"},
                                    {userName="Patrick",pass=Password "4321"}] 


//createTasks = get loginInfo >>- \loginInf -> Hint "Create tasks that will be executed" @>> enterInformation [] >>* [OnAction ActionOk (ifValue (\x -> all (\y -> all (\z -> any (\a -> a.userName == z) loginInf) y.names && y.names <> []) x) (\x -> upd (\l -> l ++ x) toDoTaks))]


createTasks = get loginInfo >>- \loginInf -> Hint "Create tasks that will be executed" @>> ((Hint "Enter topic" @>> enterInformation []) -&&- (Hint "Enter date" @>> enterInformation [])) -&&- 
                                enterMultipleChoice [ChooseFromCheckGroup (\s -> (s.Login.userName))] loginInf >>* 
                                [OnAction ActionOk (hasValue  (\z -> let u = map (\((a,b),c) -> let x = map (\y -> y.userName) c in {todo=a,limit=b,names=x}) z in upd (\l -> u ++ l) toDoTaks ))]
                                                //(\z -> all (\((_,_),c) -> c <> []) z  )
succesFull  = (viewInformation [] "Succesfully logged in")

accountCreated  = (viewInformation [] "New account created")

error = (viewInformation [] "Wrong password given")

//login :: Task [Login]
login = Hint "Enter your username and password" @>> enterInformation [] >>* [OnAction (Action "Login") (hasValue (correctLogin loginInfo))]
    where correctLogin logins x = get logins >>- \loginInfo -> if (isMember x loginInfo) (succesFull)  (if (any (\a -> x.userName == a.userName) loginInfo) ( error) (upd (\l -> [x:l]) logins ||- accountCreated))




//loginMain :: Task [Login]
//loginMain = withShared loginInfo login

Start w = doTasks createTasks w