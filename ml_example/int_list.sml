datatype mylist = Nil | Cons of int * mylist

fun printAll xs = case xs of
                   Nil => ()
                 | Cons (x, xs) => let val () = print x
                                   in printAll xs end

val list = Cons (1, Cons (2, Cons (3, Nil)))
val () = printAll list
