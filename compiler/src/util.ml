let strip_last_char s =
    if s = "" then "" else
    String.sub s 0 (String.length s - 1)