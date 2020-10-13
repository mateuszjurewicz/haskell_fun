doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = (if x > 10
    then x
    else x*2)
    +1
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
factorial :: Integer -> Integer
factorial n = product [1..n]
factorial' :: Int -> Int
factorial' x = product[1..x]
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY SEVEN"
lucky x = "Sorry, no luck"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not in 1 - 5!"

factorialRecursive :: (Integral a) => a -> a
factorialRecursive 0 = 1
factorialRecursive n = n * factorialRecursive (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

firstT :: (a, a, a) -> a
firstT (x, _, _) = x

secondT :: (a, a, a) -> a
secondT (_, x, _) = x

thirdT :: (a, a, a) -> a
thirdT (_, _, x) = x

head' :: [a] -> a
head' [] = error "Sorry, no head of an empy list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has 2 elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. First two elements are: " ++ show x ++ " and " ++ show y

length2 :: [a] -> Integer
length2 [] = 0
length2 (_:xs) = 1 + length2(xs)

sum2 :: (Num a) => [a] -> a
sum2 [] = 0
sum2 (f:xs) = f + sum2 xs

firstAndRest :: String -> String
firstAndRest "" = "Empty string"
firstAndRest whole@(x:xs) = "The first letter of *" ++ whole ++ "* is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Thin"
    | bmi <= 25.0 = "Medium"
    | bmi <= 30.0 = "Faet"
    | otherwise = "Whale"

compare' :: (Ord a) => a -> a -> Ordering
compare' a b 
    | a > b = GT
    | b > a = LT
    | b == a = EQ

compareInfix :: (Ord a) => a -> a -> Ordering
a `compareInfix` b
    | a > b = GT
    | b > a = LT
    | b == a = EQ

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!" 
    | otherwise = "A whale?"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ [l]

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2 

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "empty!"
                                                [x] -> "single elem!"
                                                [f, s] -> "made up of two elements!"
                                                xs -> "longer!"