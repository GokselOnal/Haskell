shift :: Int -> Char -> Char
shift x c | fromEnum c < 97 || fromEnum c > 122 = error "char must be in lower case form"
          | otherwise = toEnum(97 + mod (fromEnum c + x - 97) 26)