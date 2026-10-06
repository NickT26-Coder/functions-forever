-- Implement a program that utilizes at least 4 types.
-- (Example: int, float, string, boolean)

-- Showcase 2 built-in methods (per data type, total = 8) showcasing data manipulations.
-- (Example: sum, average, string, replace)

functionInt :: Int -> Int
functionInt x = product [x, x]  -- Using the built-in product function to multiply the integer by itself.

functionFloat :: Float -> Float -> Float
functionFloat float1 float2 = maximum [float1, float2] -- Using the built-in maximum function to find the larger of two floats.

functionString :: String -> String
functionString str = show (length str) -- Using the built-in length function to get the length of the string.

functionBool :: Bool -> Bool
functionBool bool = and [bool, True] -- Using the built-in and function to perform an AND operation.

main = do
    -- Demonstrating Int type
    let intValue = 10
    putStrLn ""
    putStrLn $ "Original Int: " ++ show intValue
    putStrLn $ "Product of Int (multiplied by itself): " ++ show (functionInt intValue)

    -- Demonstrating Float type
    let floatValue = [20.0, 40.0]
    putStrLn ""
    putStrLn $ "Original Float: " ++ show floatValue 
    putStrLn $ "Maximum Float: " ++ show (functionFloat (head floatValue) (last floatValue))

    -- Demonstrating String type
    let stringValue = "World"
    putStrLn ""
    putStrLn $ "Original String: " ++ stringValue
    putStrLn $ "The length of the string is: " ++ functionString stringValue

    -- Demonstrating Bool type
    let boolValue = False
    putStrLn ""
    putStrLn $ "Original Boolean: " ++ show boolValue
    putStrLn $ "The value of the boolean after the 'and' operation"
    putStrLn $ "(boolValue is being compared with True): " ++ show (functionBool boolValue)
    putStrLn ""