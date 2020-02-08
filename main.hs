import Expense_Calc
import System.Environment (getArgs)

main = do
    [file] <-getArgs
    s <- readFile file
    let test = print_expenses s "Josh"
    putStr $ show test
