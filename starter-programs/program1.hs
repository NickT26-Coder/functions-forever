-- Implement a program that utilizes at least 4 types.
-- (Example: int, float, string, boolean)

-- Showcase 2 built-in methods (per data type, total = 8) showcasing data manipulations.
-- (Example: sum, average, string, replace)

functionInt :: Int -> Int
functionInt x = product [x, x]  -- Using the built-in product function to multiply the integer by itself.

functionIntSum :: [Int]-> Int
functionIntSum numbers = sum numbers -- Using the built-in sum function to add integers in a list.

functionFloat :: Float -> Float -> Float
functionFloat float1 float2 = maximum [float1, float2] -- Using the built-in maximum function to find the larger of two floats.

functionFloatMin :: Float -> Float -> Float
functionFloatMin float1 float2 = minimum [float1, float2] -- Using the built-in minimum function to find the smaller of two floats.

functionString :: String -> String
functionString str = show (length str) -- Using the built-in length function to get the length of the string.

functionStringReverse :: String -> String
functionStringReverse str = reverse str -- Using the built-in reverse funtion to reverse string

functionBool :: Bool -> Bool
functionBool bool = and [bool, True] -- Using the built-in and function to perform an AND operation.

functionBoolNot :: Bool -> Bool
functionBoolNot bool = not bool -- Using the built-in not function to reverse boolean values

main = do
    -- Demonstrating Int type
    let intValue = 10
    putStrLn ""
    putStrLn $ "Original Int: " ++ show intValue
    putStrLn $ "Product of Int (multiplied by itself): " ++ show (functionInt intValue)

    -- Demonstration of sum for Int
    let intValues = [10, 20, 30]
    putStrLn $ "Original Int Values in list: " ++ show intValues
    putStrLn $ "Sum of Int Values: " ++ show (functionIntSum intValues)

    -- Demonstrating Float type
    let floatValue = [20.0, 40.0]
    putStrLn ""
    putStrLn $ "Original Float: " ++ show floatValue
    putStrLn $ "Maximum Float: " ++ show (functionFloat (head floatValue) (last floatValue))

    -- Demonstration of Float Min
    putStrLn $ "Minimum Float: " ++ show (functionFloatMin (head floatValue) (last floatValue))

    -- Demonstrating String type
    let stringValue = "World"
    putStrLn ""
    putStrLn $ "Original String: " ++ stringValue
    putStrLn $ "The length of the string is: " ++ functionString stringValue

    -- Demonstration of reverse Str
    putStrLn $ "Original String: " ++ stringValue
    putStrLn $ "Reverse String: " ++ show (functionStringReverse stringValue)

    -- Demonstrating Bool type
    let boolValue = False
    putStrLn ""
    putStrLn $ "Original Boolean: " ++ show boolValue
    putStrLn $ "The value of the boolean after the 'and' operation"
    putStrLn $ "(boolValue is being compared with True): " ++ show (functionBool boolValue)
    putStrLn ""

    -- Demostration of not function
    putStrLn $ "Original Boolean: " ++ show boolValue
    putStrLn $ "Not Boolean: " ++ show (functionBoolNot boolValue)
