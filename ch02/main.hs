-- Exercises A.

-- False :: Boolean
-- (["foo", "bar"], 'a') :: ([[Char]], Char)
-- [(True, []), (False, [['a']])] :: [(Boolean, [[Char]])]

-- Exercises B.

-- Question 1.
-- last :: [a] -> a
-- It can return an item from a specific position in the list (e.g. first, last, mid).
-- It cannot return a random item since the function must be pure.
-- It cannot return an item not in the list since it must work on any type.

-- Question 2.

lastButOne (x:[y]) = x
lastButOne (x:xs) = lastButOne (xs)

lastButOne2 xs = head ((drop 1) (reverse xs))

lastButOne3 = head.(drop 1).reverse

-- Question 3.
-- lastButOne [1] -> Non-exhaustive patterns in function lastButOne
-- lastButOne2 [2] -> Exception: Prelude.head: empty list
-- lastButOne3 is equivalent to two, but written in 