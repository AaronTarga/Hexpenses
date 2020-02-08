module File_Reader where

import Data.List.Split

data Users = UsersC [String] deriving Show

data Entries = EntriesC [[String]] deriving Show

data Expenses = ExpensesC (Users,Entries) deriving Show

expenses_state :: String -> Expenses
expenses_state s = aux_exp (splitOn "\n" s) []

aux_exp :: [String] -> [String] -> Expenses
aux_exp ("":xs) users = entry_list xs [] [[]] $ UsersC users 
aux_exp (x: xs) users = aux_exp xs $ x : users

entry_list :: [String] -> [String] -> [[String]] -> Users -> Expenses
entry_list [] [] entries users = ExpensesC(users,EntriesC (entries))
entry_list [] acc entries users = ExpensesC(users,EntriesC (reverse acc : entries))
entry_list ("":xs) acc [[]] users = entry_list xs [] [reverse acc]  users
entry_list ("":xs) acc entries users = entry_list xs [] (reverse acc : entries)  users
entry_list (x : xs) acc entries users = entry_list xs (x : acc) entries users

