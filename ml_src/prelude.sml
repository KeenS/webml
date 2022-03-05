
__lang_item((bool))__
datatype bool = false | true

__lang_item((string))__
datatype string = Char of (char * string) | Empty


__lang_item((stringEq))__
fun stringEq (Empty, Empty) = true
  | stringEq (Char(c1, s1), Char(c2, s2)) = if =(c1, c2) then stringEq (s1, s2) else false
  | stringEq _ = false

__lang_item((stringNeq))__
fun stringNeq (Empty, Empty) = false
  | stringNeq (Char(c1, s1), Char(c2, s2)) = if <>(c1, c2) then true else stringNeq(s1, s2)
  | stringNeq _ = true


__lang_item((stringGt))__
fun stringGt (Empty, Empty) = false
  | stringGt (Empty, Char _) = false
  | stringGt (Char _, Empty) = true
  | stringGt (Char(c1, s1), Char(c2, s2)) =
    if >(c1, c2)
    then true
    else if =(c1, c2)
    then stringGt(s1, s2)
    else false

__lang_item((stringGe))__
fun stringGe (Empty, Empty) = true
  | stringGe (Empty, Char _) = false
  | stringGe (Char _, Empty) = true
  | stringGe (Char(c1, s1), Char(c2, s2)) =
    if >(c1, c2)
    then true
    else if =(c1, c2)
    then stringGe(s1, s2)
    else false


__lang_item((stringLt))__
fun stringLt (Empty, Empty) = false
  | stringLt (Empty, Char _) = true
  | stringLt (Char _, Empty) = false
  | stringLt (Char(c1, s1), Char(c2, s2)) =
    if <(c1, c2)
    then true
    else if =(c1, c2)
    then stringLt(s1, s2)
    else false


__lang_item((stringLe))__
fun stringLe (Empty, Empty) = true
  | stringLe (Empty, Char _) = true
  | stringLe (Char _, Empty) = false
  | stringLe (Char(c1, s1), Char(c2, s2)) =
    if <(c1, c2)
    then true
    else if =(c1, c2)
    then stringLe(s1, s2)
    else false


val version = 100000
infix 7 * / div mod
infix 6 + -
infix 4 = <> <= < >= >


fun printChar x = _externcall("js-ffi"."print": (char) -> unit)(x)
fun print Empty = ()
  | print (Char(c, s)) = let val () = printChar c
                         in print s end

fun digitToChar 0 = #"0"
  | digitToChar 1 = #"1"
  | digitToChar 2 = #"2"
  | digitToChar 3 = #"3"
  | digitToChar 4 = #"4"
  | digitToChar 5 = #"5"
  | digitToChar 6 = #"6"
  | digitToChar 7 = #"7"
  | digitToChar 8 = #"8"
  | digitToChar 9 = #"9"
  (* unreachable *)
  | digitToChar _ = #" "

fun stringFromIntInner d acc = let
    val c = digitToChar(d mod 10)
    val rest = d div 10
    val acc = Char(c,acc)
in
    if rest = 0
    then acc
    else stringFromIntInner rest acc
end


fun stringFromInt 0 = "0"
  | stringFromInt d = stringFromIntInner d Empty


fun printInt i = let
    val () = print (stringFromInt i)
in print "\n" end
