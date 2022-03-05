val s = "This is a string literal\a\b\t\n\v\f\r\\\""
val x = case "s" of
            "s" => print "true\n"
          | _ => print "false"
val y = case "hello" of
            "goodby" => print "goodby\n"
          | "hello" => print "hello\n"
          | _ => print "other\n"

val z = if "hello" = "goodby"
        then print "true\n"
        else print "false\n"
val z = if "hello" = "hello"
        then print "true\n"
        else print "false\n"

val a = case ("a" < "ab", "c" <= "c", "df" > "de", "gh" >= "gh", "i" <> "j") of
            (true, true, true, true, true) => print "true\n"
          | _ => print "false\n"
val b = case ("ab" < "a", "d" <= "c", "de" > "df", "gh" >= "gi", "j" <> "j") of
            (false, false, false, false, false) => print "true\n"
          | _ => print "false\n"

