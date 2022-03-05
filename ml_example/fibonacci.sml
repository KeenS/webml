fun fib n = if n < 2
            then 1
            else fib (n - 1) + fib (n - 2)
val a = printInt (fib 0)
val a = printInt (fib 1)
val a = printInt (fib 2)
val a = printInt (fib 3)
val a = printInt (fib 4)
val a = printInt (fib 5)
