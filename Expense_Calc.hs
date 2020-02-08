module Expense_Calc where

import File_Reader
import Data.List.Split

data Moneydiff = MoneydiffC [(String,Double)] deriving Show

print_expenses ::  String -> String -> String
print_expenses input user = let 
    ExpensesC(users,entries) = expenses_state input
    positives = user_entries user entries [[]]
    in if (user `elem` users) then show (calculate_positive user users positives) else "Enter a valid user!\n"


calculate_positive :: String -> [String] -> [[String]] -> Moneydiff
calculate_positive user users entries = calculate_positive_aux user users entries [("",0)]


calculate_positive_aux :: String -> [String] -> [[String]] -> [(String,Double)] -> Moneydiff
calculate_positive_aux _ _ [] diff = MoneydiffC diff
calculate_positive_aux user users entries (("",0):_) = calculate_positive_aux user users entries $ foldr (\x xs -> ((x,0.0):xs) ) [] (filter (\x -> x /= user) users)
calculate_positive_aux user users ((_:price:us):xss) diff = calculate_positive_aux user users xss (map (\(x, y) -> if x `elem` (get_users (head us)) then (x,y + (read price :: Double) / fromIntegral (length (get_users (head us)))) else (x,y)) diff )


--gets all entries from a specific user
user_entries :: String -> [[String]] -> [[String]] -> [[String]]
user_entries user [] diff = diff
user_entries user ((x: xs):xss) diff = if x == user then user_checked user ((tail xs):xss) diff else user_entries user xss diff

user_checked :: String -> [[String]] -> [[String]] -> [[String]]
user_checked user ([]:xss) diff = user_entries user xss diff
user_checked user ((x:xs):xss) [[]] = user_checked user (xs:xss) [splitOn ";" x] -- to get rid off empty list
user_checked user ((x:xs):xss) diff = user_checked user (xs:xss) (splitOn ";" x : diff)

get_users :: String -> [String]
get_users s = splitOn "," s