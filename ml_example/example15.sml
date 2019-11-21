datatype option = SOME of int | NONE

fun isSome opt = case opt of
                     SOME x => true
                   | NONE => false
