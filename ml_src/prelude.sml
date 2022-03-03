__lang_item((bool))__
datatype bool = false | true
__lang_item((string))__
datatype string = Char of (char * string) | Empty

val version = 100000
fun print x = _externcall("js-ffi"."print": (int) -> unit)(x)
infix 7 * / div mod
infix 6 + -
infix 4 = <> <= < >= >
