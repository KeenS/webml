datatype option = SOME of int | NONE

fun xor (NONE, NONE) = NONE
  | xor (SOME _, SOME _) = NONE
  | xor (SOME x, _) = SOME x
  | xor (_, SOME x) = SOME x


