module File_Reader where

import Data.List.Split

data Expenses = ExpensesC ([String],[[String]]) deriving Show

-- reads File and converts it to Expenses Format
expenses_state :: String -> Expenses
expenses_state s = aux_exp (splitOn "\n" s) []

--puts all users in list
aux_exp :: [String] -> [String] -> Expenses
aux_exp ("":xs) users = entry_list xs [] [[]] users
aux_exp (x: xs) users = aux_exp xs (x : users)

--puts all entries in list
entry_list :: [String] -> [String] -> [[String]] -> [String] -> Expenses
entry_list [] [] entries users = ExpensesC(users,entries)
entry_list [] acc entries users = ExpensesC(users,(reverse acc : entries))
entry_list ("":xs) acc [[]] users = entry_list xs [] [reverse acc]  users
entry_list ("":xs) acc entries users = entry_list xs [] (reverse acc : entries)  users
entry_list (x : xs) acc entries users = entry_list xs (x : acc) entries users

