val x = 1
val y=false
val z = y
val b = 1 + 2 * 3 - 4
val c = (1 + 2) * 3 + 4
val d = fn x => fn y => x + y
val e = if true then b else c
val f = let val d = 1 in d + c end
val g = d 1 2
val h = let val y = 1 in fn x => x + y end
val i = let val two = 2 fun cls1 x = x + 1 fun cls2  x = x + two in if true then cls1 else cls2 end
val j = (1, 2, 3)
val a = i 2
