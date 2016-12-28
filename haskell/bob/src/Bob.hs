module Bob (responseFor) where

capCounter :: String -> Integer
capCounter = foldl (\s x -> if x `elem` ['A'..'Z'] then s + 1 else s) 0

hasLower :: String -> Bool
hasLower v = foldl (\s x -> if x `elem` ['a'..'z'] then s + 1 else s) 0 v > 0

responseFor :: String -> String
responseFor x
  | x == "Bob" || null (words x) = "Fine. Be that way!"
  | any (>= 2) (map capCounter (words x)) && not (hasLower x) = "Whoa, chill out!"
  | last (last (words x)) == '?' = "Sure."
  | otherwise = "Whatever."
