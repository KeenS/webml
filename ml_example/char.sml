val () = if #"a" = #"b"
         then print 1
         else print 0
val () = case #"b" of
             #"a" => print 0
           | #"b" => print 1
           | _ => print 2
