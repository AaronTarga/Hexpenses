import Expense_Calc
import System.Environment (getArgs)
import System.IO
import Data.List.Split

main = do
    hSetBuffering stdout NoBuffering
    [file] <-getArgs
    s <- readFile file
    putStr "Actions: [p]rint expenses for user [e]xit: "
    option <- getLine
    case option of
        -- "a" -> add_entry s file
        "p" -> print_user s
        "e" -> return()
        _ -> main

print_user s = do 
    putStr "Username: "
    name <- getLine
    let test = print_expenses s name
    putStr $ show test ++ "\n"
    main

-- add_entry file s = do
--     putStr "Owner: "
--     name <- getLine
--     putStr "Date: "
--     date <- getLine
--     add_items file name date [] s

-- add_items file owner date items s = do
--     putStr "Enter items like this (name;price;person1,person2):"
--     item <- getLine
--     if (length (splitOn ";" item) == 3) then 
--         if items == [] then add_items file owner date [item] s
--         else add_items file owner date (item:items) s
--     else if items /= [] then (save file owner date items s) else main

-- format_items [] res = res   
-- format_items (x:xs) res = format_items xs (res ++ show x ++ "\n")  

-- save file owner date items s = do
--     let new = s ++ "\n\n" ++ show owner ++ "\n" ++ date ++ "\n" ++ format_items items ""
--     writeFile file new
--     main