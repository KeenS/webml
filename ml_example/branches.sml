val x = case true of
            false => true
          | y => (case y of
                      true => true
                   | _ => false)
val y = if x
        then print "true\n"
        else print "false\n"
