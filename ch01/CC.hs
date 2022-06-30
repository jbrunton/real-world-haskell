main = interact charCount
    where charCount input = show (sum $ map length (lines input)) ++ "\n"
