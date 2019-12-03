datatype option = SOME of int | NONE

fun xor (o1, o2) = case (o1, o2) of
                    (NONE, NONE) => NONE
                  | (SOME _, SOME _) => NONE
                  | (SOME x, _) => SOME x
                  | (_, SOME x) => SOME x


