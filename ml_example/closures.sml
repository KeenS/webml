val i = let val two = 2 fun cls1 x = x + 1 fun cls2  x = x + two in if true then cls1 else cls2 end
val a = i 2
val b = printInt a
