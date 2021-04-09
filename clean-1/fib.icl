module fib

import StdEnv

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

Start :: Int
Start = fib 35