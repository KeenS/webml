val () = if #"a" = #"b"
         then print "true\n"
         else print "false\n"
val () = case #"b" of
             #"a" => print "b is a\n"
           | #"b" => print "b is b\n"
           | _ => print "other\n"
