-- Automata in Haskell
-- Conway's Game of Life
-- For refactor: include BoardSize as part of Board data type

import Data.List
import System.Random
import UI.NCurses

type Cell = (Int, Int) 

type Board = [Cell]

type BoardSize = (Int, Int)

emptyBoard :: BoardSize -> Board
emptyBoard (rows, cols) = [(i,j) | i <- [1.. rows], j <- [1.. cols]]

randomBoard :: BoardSize -> IO Board
randomBoard (rows, cols) = do
    let limit = rows * cols `div` 4
    g <- newStdGen
    g' <- newStdGen
    return $ nub $ zip (take limit (randomRs (1, rows) g)) (take limit (randomRs (1, cols) g'))

-- Board Representation Functions
boardToString :: BoardSize -> Board -> String
boardToString (rows, cols) board = addEndlines cols $ map
    (\x -> if x `elem` board then '*'  else ' ') 
    (emptyBoard (rows, cols)) 

addEndlines :: Int -> String -> String
addEndlines _  ""  = ""
addEndlines cols xs = (take cols xs) ++ "\n" ++ (addEndlines cols $ drop cols xs)
-- is it bad to use take and drop?

-- Find all possible neighbors, including cell passed to function,
possibleCells :: BoardSize -> Cell -> [(Int, Int)]
possibleCells (rows, cols) (row, col) = 
    [(i,j) |
    i <- [row - 1 .. row + 1], 
    j <- [col - 1 .. col + 1],
    1 <= i && i <= rows,
    1 <= j && j <= cols]

allPossibleCells :: BoardSize -> Board -> Board 
allPossibleCells (rows, cols) = nub . concat . map (possibleCells (rows, cols))

-- count neighbors (sum $ map can be foldr?) refactor into applyRules?
countNeighbors :: BoardSize -> Cell -> Board -> Int
countNeighbors (rows, cols) cell currentBoard = 
    sum $ map (\n -> if n `elem` currentBoard then 1 else 0)
        (filter (/= cell) (possibleCells (rows, cols) cell))

-- apply rules
applyRules :: BoardSize -> Board -> Cell -> Bool
applyRules (rows, cols) currentBoard cell
    | count == 2 && cell `elem` currentBoard = True
    | count == 3 = True
    | otherwise = False
    where count = countNeighbors (rows, cols) cell currentBoard

-- iterate
iterateAlive :: BoardSize -> Board -> Board
iterateAlive (rows, cols) currentBoard = 
    filter (applyRules (rows, cols) currentBoard) 
        (allPossibleCells (rows, cols) currentBoard)

main:: IO ()
main = do
    board <- randomBoard (30,30)
    runCurses $ do
        w <- defaultWindow
        updateWindow w $ do
            drawString $ boardToString (30,30) board
        render
        drawNextBoard board w

--IO stuff

drawNextBoard :: Board -> Window -> Curses ()
drawNextBoard currentBoard w = do
    updateWindow w $ do
        moveCursor 0 0
        drawString $ boardToString (30,30) currentBoard
    render
    waitFor w (\ev -> ev == EventSpecialKey KeyRightArrow)
    drawNextBoard (iterateAlive (30,30) currentBoard) w

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do 
        ev <- getEvent w Nothing
        case ev of 
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
