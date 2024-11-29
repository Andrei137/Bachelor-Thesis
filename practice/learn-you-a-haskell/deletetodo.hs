import System.IO
import System.Directory
import Data.List
import Control.Exception

main = do
    let filePath = "txt/todo.txt"
    content <- readFile filePath
    let todoTasks = lines content
        numberedTasks = zipWith (\idx line -> "[" ++ show idx ++ "] " ++ line) 
                            [1..] todoTasks
    putStrLn "There are your TODOs:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString - 1
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName
        )
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile filePath
            renameFile tempName filePath
        )
