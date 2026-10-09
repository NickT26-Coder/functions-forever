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
    
-- TUPLES

-- Need to specify the type of each element in the tuple unlike lists
item :: (String, Int) -> String
item (item, _) = "Current Item: " ++ item

matrixMultiplication :: (Int, Int) -> (Int, Int) -> (Int, Int, Int, Int)
matrixMultiplication (r1, c1) (r2, c2) = (r1 * c1, r1 * c2, r2 * c1, r2 * c2)

testTuples :: IO()
testTuples = do
    -- A tuple is typically used when you know beforehand how many elements you want to include and tuples are not homogenous. 
    -- This size restriction for tuples is typically why tuples are used as "pairs" or "triples"

    putStrLn $ "Get first element (name) in pair: " ++ show (item ("Apple", 3))
    -- putStrLn $ show (item ("Apple", 3, 3)) Only works for pairs with a String and Int

    {- 
    A pair and a triple are separate data types unlike a list of size 2 and a list of size 3
    If you use the command, ":t (1,2)" it returns: (Num a, Num b) => (a, b)
    If you use the command, ":t (1,2,3)" it returns: (Num a, Num b, Num c) => (a, b, c)
    If you use the command, ":t [1,2]" it returns: Num a => [a]
    If you use the command, ":t [1,2,3]" it returns: Num a => [a]
    -}

    -- When creating a list of tuples the first element creates a precendence. In this case the first element in a pair
    let lst = [(1,2), (1,3)] -- This is a valid list
    -- let lst1 = [(1,2), (1,2,3)] -- This is not
    putStrLn $ "Valid list of tuples: " ++ show lst

    -- 2 x 1 and 1 x 2 Matrix Multiplication Example
    putStrLn $ "2x1 multiplied by 1x2 matrix creates a 2x2 matrix: " ++ show (matrixMultiplication (5, 4) (4, 5))

-- GUARDS

gravityGuard :: (RealFloat a) => a -> String
gravityGuard val
    | val < 0.000000000066743 = "Too Weak"
    | val > 0.000000000066743 = "Too Strong"
    | otherwise = "Perfect Gravitational Constant Value" -- Used as a fail safe like "default:" in Java

gravityGuardClause :: (RealFloat a) => a -> String
gravityGuardClause val
    | val < gConstant = "Too Weak"
    | val > gConstant = "Too Strong"
    | otherwise = "Perfect Gravitational Constant Value" 
    where gConstant = 0.000000000066743 -- The where clause allows us to create a variable for the function and guards to use

testGuards :: IO()
testGuards = do
    -- A guard is similar to an if statement or switch statement that uses pipes to denote each part of the statement

    putStrLn $ "Check Gravity Level: " ++ show (gravityGuard 1)

-- Pattern Matching