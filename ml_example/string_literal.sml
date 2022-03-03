val s = "This is a string literal\a\b\t\n\v\f\r\\\""
val x = case "s" of
            "s" => print 0
          | _ => print 1
val x = case "hello" of
            "goodby" => print 0
          | "hello" => print 1
          | _ => print 2
