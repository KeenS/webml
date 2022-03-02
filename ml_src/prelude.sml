__lang_item((bool))__
datatype bool = false | true

val version = 100000
fun print x = _externcall("js-ffi"."print": (int) -> unit)(x)
infix 7 * / div mod
infix 6 + -
infix 4 = <> <= < >= >
datatype string = Char of (char * string) | Empty
