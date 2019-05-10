
-- how do i sort an array?
sort :: Array Int Int -> Array Int Int
sort = undefined

testSort :: [Array Int Int] -> [Array Int Int]

-- recursive definition with go/helper method
binSearch :: Array Int Int -> Int -> Maybe Int
binSearch arr target = uncurry (go arr) (bounds arr)

testBinSearch :: [(Array Int Int, Int)] -> 

-- find two elems of nums which add up to target
twoSum :: [Int] -> Int -> Maybe (Int,Int)
twoSum nums target = undefined

testTwoSum :: [([Int], Int)] -> [(([Int],Int), Maybe Int)]
testTwoSum = undefined

-- take the test method and data
-- use a typeclass? Tester?
-- Input thing, Output thing
runTester f 

main = do
    putStrLn "hello world"
