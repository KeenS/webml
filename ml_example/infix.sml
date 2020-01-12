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
