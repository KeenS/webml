val a = let
  val b = let
    val c = 1
    val d = 2
  in
    c + d * 3 + 4
  end
  val e = if let val f = true in f end
          then let val g = true in g end
          else let val h = false in h end
in
 (let
    val i = fn x => fn y => x + y
  in
    i
  end) (let
    val j = b + b
  in
   j
  end) 2
end
val k = print a
