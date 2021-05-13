// Gianni Monteban - Martijn Vogelaar
// 1047546 -1047391

module clean5

import iTasks

import iTasks.Extensions.DateTime

import Data.Either

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

instance == ToDo where
    (==) l1 l2 = (l1.todo == l2.todo) && (l1.limit == l2.limit) && (l1.names == l2.names)


derive class iTask Login, ToDo

derive class Eq Date

toDoTaks = sharedStore "ToDo" []

loginInfo = sharedStore "myLogins" [{userName="Jan",pass=Password "1234"},
                                    {userName="Patrick",pass=Password "4321"}] 


createTasks = get loginInfo >>- \loginInf -> Hint "Create tasks that will be executed" @>> ((Hint "Enter topic" @>> enterInformation []) -&&- (Hint "Enter date" @>> enterInformation [])) -&&- 
                                enterMultipleChoice [ChooseFromCheckGroup (\s -> (s.Login.userName))] loginInf >>* 
                                [OnAction ActionOk (hasValue  (\z -> let u = (\((a,b),c) -> let x = map (\y -> y.userName) c in {todo=a,limit=b,names=x}) z in upd (\l -> [u :l]) toDoTaks ))]

assignedTasks name = get toDoTaks >>- \tasks -> Hint "Execute one of the following tasks" @>> enterChoice [ChooseFromCheckGroup (\s -> (s.ToDo.todo))] (filter (\x -> isMember name x.names) tasks)
                                            >>* [OnAction ActionOk (hasValue (\z -> upd (\y -> filter (\x -> x <> z) y) toDoTaks ))]


login = Hint "Enter your username and password" @>> enterInformation [] >>* [OnAction (Action "Login") (hasValue (correctLogin loginInfo))]
    where correctLogin logins x = get logins >>- \loginInfo -> if (isMember x loginInfo) (loggedIn x.userName)  (if (any (\a -> x.userName == a.userName) loginInfo) (login) (upd (\l -> [x:l]) logins ||- (loggedIn x.userName)))

logout = viewInformation [] "Logout" >>? return o ?Just

logoutScreen = viewInformation [] "You are now logged out!"

loggedIn name = eitherTask (createTasks -||- (assignedTasks name)) (logout) >>- \x -> case x of 
                                                                                        Left a = loggedIn name
                                                                                        Right a = logoutScreen


Start w = doTasks login w