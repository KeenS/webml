datatype ord = GREATER | EQUAL | LESS

fun compare x y = if x - y < 0
                  then LESS
                  else if x - y = 0
                  then EQUAL
                  else GREATER
