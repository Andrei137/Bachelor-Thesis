import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

onError :: FilePath -> Handle -> IO ()
onError tempName tempHandle = do
    hClose tempHandle
    removeFile tempName

onSuccess :: FilePath -> Handle -> FilePath -> String -> IO ()
onSuccess tempName tempHandle filePath newTodoItems = do
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile filePath
    renameFile tempName filePath

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "The " ++ command ++ " command doesn't exist"

wrongNoArgs :: String -> Int -> IO ()
wrongNoArgs command number = 
    putStrLn $ "The " ++ command ++ " takes exactly " ++ show number ++ argsString
    where 
        argsString = if number == 1 then " argument" else " arguments"

numberedList :: [String] -> [String]
numberedList l = zipWith (\idx line -> "[" ++ show idx ++ "] " ++ line) [1..] l

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile ("txt/" ++ fileName) (todoItem ++ "\n")
add _ = wrongNoArgs "add" 2

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    let filePath = "txt/" ++ fileName
    content <- readFile (filePath)
    let todoTasks = lines content
        selectedTask = todoTasks !! (read numberString - 1)
        newTodoItems = unlines $ selectedTask : delete selectedTask todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            onError tempName tempHandle
        )
        (\(tempName, tempHandle) -> do
            onSuccess tempName tempHandle filePath newTodoItems
        )
bump _ = wrongNoArgs "bump" 2

view :: [String] -> IO ()
view [fileName] = do
    content <- readFile ("txt/" ++ fileName)
    putStr . unlines . numberedList $ lines content
view _ = wrongNoArgs "view" 1

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    let filePath = "txt/" ++ fileName
    content <- readFile (filePath)
    let todoTasks = lines content
        newTodoItems = unlines $ delete (todoTasks !! (read numberString - 1)) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            onError tempName tempHandle
        )
        (\(tempName, tempHandle) -> do
            onSuccess tempName tempHandle filePath newTodoItems
        )
remove _ = wrongNoArgs "remove" 2

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "bump" = bump
dispatch "view" = view
dispatch "remove" = remove
dispatch command = doesntExist command

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Please specify a command"
        (command:argList) -> dispatch command argList
