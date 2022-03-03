val s = "This is a string literal\a\b\t\n\v\f\r\\\""
val x = case "s" of
            "s" => print 0
          | _ => print 1
val y = case "hello" of
            "goodby" => print 0
          | "hello" => print 1
          | _ => print 2

val z = if "hello" = "goodby"
        then print 0
        else print 1
val z = if "hello" = "hello"
        then print 0
        else print 1

val a = case ("a" < "ab", "c" <= "c", "df" > "de", "gh" >= "gh", "i" <> "j") of
            (true, true, true, true, true) => print 0
          | _ => print 1
val b = case ("ab" < "a", "d" <= "c", "de" > "df", "gh" >= "gi", "j" <> "j") of
            (false, false, false, false, false) => print 0
          | _ => print 1

