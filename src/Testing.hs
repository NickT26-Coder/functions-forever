import Runtime

testPass :: String -> Value -> Bool
testPass code expected =
  case evalProgram newEnvironment code of
    Right (_, actual) -> actual == expected
    Left _ -> False

testFails :: String -> Bool
testFails code =
  case evalProgram newEnvironment code of
    Left _ -> True
    Right _ -> False

tests :: [Bool]
tests =
  [ testPass "2 + 3 * 4" (VNumber 14),
    testFails "10 / 0",
    testPass "let x = 2;" (VNothing),
    testPass "let x = 2; x = 3;" (VNumber 3)
  ]
