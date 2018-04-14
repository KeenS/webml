val x = case true of
            false => true
          | y => (case y of
                     false => true
                   | _ => true)
val y = if x
        then print 1
        else print 0
