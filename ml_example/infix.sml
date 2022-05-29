fun add(x, y) =  x + y

infix 6 add

val x = 1 add 2 * 3

fun sub(x, y) =  x - y

val y = let
    infix 6 sub
in
    2 sub 3 div 2
end

val z = sub (2, 3)

datatype intlist = :: of int * intlist | nil
infixr 7 ::

val list = 1 :: 2 :: 3 :: nil
val b = case list of
            1 :: 2 :: 3 :: nil => print "true\n"
         | _ => print "false\n"
nonfix add

val a = add(1, 2)
