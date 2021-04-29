module skeleton9

import iTasks

/*
	Pieter Koopman, pieter@cs.ru.nl
	Advanced Programming. Skeleton for assignment 9 in 20-21.
	Use this in a project with environment iTasks.
*/

:: Student =
	{ name :: String
	, snum :: Int
	, bama :: BaMa
	, year :: Int
	}

:: BaMa = Bachelor | Master
derive class iTask Student, BaMa
Start w = doTasks task11 w

task :: Task [Int]
task = enterInformation []

task1 :: Task Student
task1 = enterInformation []

task2 :: Task [Student]
task2 = enterInformation []

task3 :: Task Student
task3 = updateInformation [] student

task4 :: Task [Student]
task4 = updateInformation [] students

task5 :: Task Student
task5 = Hint "Choose favorite student:" @>> enterChoice [ChooseFromDropdown \s -> s.Student.name] students

task6 :: Task [Student]
task6 = Hint "Choose partners:" @>> enterMultipleChoice [ChooseFromCheckGroup \s -> (s.Student.name, s.Student.bama)] students

task7helper :: Task [Student]
task7helper = task6 >>* [OnAction ActionOk (ifValue (\x -> (not (isEmpty x))) (\x -> return x )),
						OnAction ActionCancel (always (return []))]



task7 :: Task [Student]
task7 = task7helper >>- \s.viewInformation [ViewAs (\x-> (length x, (map (\z-> z.Student.name) x)))] s <<@ Title "Number of students and which students"

task8run :: Student -> Task Student
task8run x = updateInformation [UpdateAs (\y -> y.Student.name) (\y z -> {name = z,snum = y.Student.snum,bama = y.Student.bama, year = y.Student.year} )] x -|| viewInformation[ViewAs (\y -> (y.Student.snum, y.Student.bama,y.Student.year))] x 


task8 :: Task Student
task8 = task8run student >>* [OnAction (Action "Update") (hasValue (\x -> return x))] >>- \s.viewInformation [] s <<@ Title "Updated student" 


task9 :: Task Student
task9 =  ((task1) -||- (Hint "Choose default student:" @>> enterChoice [ChooseFromDropdown \s -> s.Student.name] students))
			>>*  [OnAction (Action "Submit") (hasValue (\x -> return x))] >>- \s.viewInformation [] s <<@ Title "Student" 


studentName :: Task String
studentName = Hint "Enter a student name: " @>> enterInformation [] 

studentSnum :: Task Int
studentSnum = Hint "Enter a student number: " @>> enterInformation [] 

studentBaMa :: Task BaMa
studentBaMa = Hint "Enter the bama: " @>> enterInformation [] 

studentYear :: Task Int
studentYear = Hint "Enter year: " @>> enterInformation [] 


task10 :: Task Student
task10 = (studentName -&&- studentSnum) -&&- (studentBaMa -&&- studentYear) 
		>>*  [OnAction (Action "Submit") (hasValue (\((a,b),(c,d)) -> return {name = a,snum = b, bama = c, year = d}))] >>- \s.viewInformation [] s <<@ Title "Student" 

updateBool :: Bool -> Task Bool
updateBool x = updateChoice [ChooseFromDropdown (\y -> y)] [True,False] x

task11 :: Task Bool
task11 = updateBool False >>* [OnAction (Action "Update") (hasValue (\x -> return x))] >>- \s.viewInformation [ViewAs \z -> case z of
																													True -> "True"
																													False -> "False"] s

students :: [Student]
students =
	[{name = "Alice"
	 ,snum = 1000
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Bob"
	 ,snum = 1003
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Carol"
	 ,snum = 1024
	 ,bama = Master
	 ,year = 2
	 }
	,{name = "Dave"
	 ,snum = 2048
	 ,bama = Bachelor
	 ,year = 1
	 }
	,{name = "Eve"
	 ,snum = 4096
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Frank"
	 ,snum = 1023
	 ,bama = Master
	 ,year = 1
	 }
	]

student :: Student
student = students !! 0
