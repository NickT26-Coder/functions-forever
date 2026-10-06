{-
Demonstrate the use of two data structures and two control structures.

Data Structures:
- Lists
- Tuples

Control Structures:
- Guards
- Pattern Matching
-}

-- LISTS

{- 
List comprehensions are a functional way of using lists
'*' => Optional parameter
Syntax: [generator (operation performed on each item) | item <- source, *item2 <- source2, *condition, *condition2] 
-}
oddsGtrNine :: [Int] -> [Int]
oddsGtrNine lst = [x | x <- lst, x > 9, odd x]

-- Example with generator, the generator doesn't have side effects (lst doesn't change permanently)
makePlural :: [String] -> [String]
makePlural lst = [s ++ "s"| s <- lst]

-- Example with nested lists
gatherEvens :: [[Int]] -> [[Int]]
gatherEvens lsts = [[n | n <- lst, even n] | lst <- lsts]

testList :: IO()
testList = do
    let groceries = ["Milk", "Eggs", "Cereal", "Bananas"]
    putStrLn $ "Basic List " ++ show groceries
    
    -- Unlike Python, lists are homogenous. Addiontally Strings are a list of chars [Char]
    -- This line would throw an error -> let mixedList = ["Apple", 2, False]

    -- Similar to slicing in Python, Haskell uses "ranges" to create new list quickly
    let range = [1..10]
    let arange = ['A'..'Z']
    putStrLn $ "1-10: " ++ show range ++ " A-Z: " ++ show arange

    putStrLn $ "List of Odd Integers greater than 9 from 0-50 " ++ show (oddsGtrNine [0..50])

    putStrLn $ "Makes each word plural " ++ show (makePlural ["Apple", "Orange", "Grape"])

    putStrLn $ "List of Lists " ++ show (gatherEvens [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]])
-- Tuples

-- Guards

-- Pattern Matching