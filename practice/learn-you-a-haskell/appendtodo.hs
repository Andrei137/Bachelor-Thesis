import System.IO

main = do
    todoItem <- getLine
    appendFile "txt/todo.txt" (todoItem ++ "\n")
