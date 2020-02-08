import File_Reader
import System.Environment (getArgs)

main = do
    [file] <-getArgs
    s <- readFile file
    let test = expenses_state s
    putStr $ show test
