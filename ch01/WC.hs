main = interact wordCount
    where wordCount input = show (sum $ map (length.words) (lines input)) ++ "\n"
