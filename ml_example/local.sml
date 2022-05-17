val x = 1
val y = x + 1

local
    val x = 2
in
val y = x * 2
end

val z = case y of
            4 => print "true\n"
          | 2 => print "false\n"
          | _ => print "unexpected\n"
