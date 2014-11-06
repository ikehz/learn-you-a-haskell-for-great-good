main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\l -> if isPalindrome l then "palindrome" else "not palindrome") . lines
    where isPalindrome l = l == reverse l
