val x = true andalso false orelse true
val () = if x
         then print "ok\n"
         else print "ng\n"
