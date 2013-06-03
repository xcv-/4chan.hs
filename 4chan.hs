import System.IO
import System.Environment

import FourChan.Formatable
import FourChan.Post
import FourChan.Thread
import FourChan.Board
import FourChan.Helpers.StringPiece


main :: IO ()
main = do
    args <- getArgs
    action <- if length args == 0
                  then ask "Action"
                  else return (head args)

    case action of
        "boards"  -> runBoards
        "board"   -> runBoard
        "threads" -> runThreads
        "thread"  -> runThread
        "fmtpost" -> runFmtPost

ask :: String -> IO String
ask question = do
    putStr question
    putStr "? "
    hFlush stdout
    getLine

runBoards :: IO ()
runBoards = do
    boardsInfo <- getBoardsInfo
    mapM_ print boardsInfo

runBoard :: IO ()
runBoard = do
    board <- ask "Board"
    boardInfo <- getBoardInfo board
    print boardInfo

runThreads :: IO ()
runThreads = do
    board <- ask "Board"
    threadIndex <- getThreadIndex board
    let ops = map getOp threadIndex
    mapM_ print ops

runThread :: IO ()
runThread = do
    board <- ask "Board"
    threadId <- fmap read $ ask "Thread ID"
    thread <- getThread board threadId
    mapM_ print (getPosts thread)

runFmtPost :: IO ()
runFmtPost = do
    board <- ask "Board"
    threadId <- fmap read $ ask "Thread ID"
    postId <- fmap read $ ask "Post ID"
    fmt <- ask "Format"

    thread <- getThread board threadId
    let post = head . filter (\p -> getPostId p == postId) . getPosts $ thread

    mapM_ putStrLn . piecesLines . format fmt $ post
