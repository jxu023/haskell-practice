import Data.Array
import Data.List (find)
-- import Debug.Trace

mkArrH :: Int -> Int -> [Int] -> Array Int Int
mkArrH l r vals = array (l, r) $ zip [0..r] vals
mkArr :: Int -> [Int] -> Array Int Int
mkArr n vals = mkArrH 0 (n-1) vals

pArr  :: Array Int Int -> IO ()
pArr a = print [a ! i | i <- [(fst bnd)..(snd bnd)]]
        where bnd = bounds a

msort :: Array Int Int -> Array Int Int
msort arr = go (bounds arr)
        where go (lo, hi) | lo < hi   = merge (go (lo, mid)) (go (mid + 1, hi))
                          | lo == hi  = array (lo, hi) [(lo, arr ! lo)]
                          | otherwise = array (lo, hi) []
                      where mid = div (lo + hi) 2

merge :: Array Int Int -> Array Int Int -> Array Int Int
merge a1 a2 = array (start, end) vals
        where start = min s1 s2
              end = max e1 e2
              vals = zip [start..end] $ go s1 s2
              (s1, e1) = bounds a1
              (s2, e2) = bounds a2
              go s1' s2' | s1' <= e1 && s2' <= e2 = let v1 = a1 ! s1'
                                                        v2 = a2 ! s2'
                                                    in if v1 < v2
                                                          then v1:(go (s1' + 1) s2')
                                                          else v2:(go s1' (s2' + 1))
                         | s1' <= e1              = [a1 ! i | i <- [s1'..e1]]
                         | otherwise              = [a2 ! i | i <- [s2'..e2]]

testSort :: [Array Int Int] -> [Array Int Int]
testSort = undefined

-- recursive definition with go/helper method
binSearch :: Array Int Int -> Int -> Maybe Int
binSearch arr target = uncurry go (bounds arr)
        where go lo hi | lo > hi         = Nothing
                       | midval < target = go (mid + 1) hi
                       | midval > target = go lo (mid - 1)
                       | otherwise       = Just mid
                      where mid = div (lo + hi) 2
                            midval = arr ! mid

testBinSearch :: [(Array Int Int, Int)] -> [(Array Int Int, Int, Int)]
testBinSearch = undefined

-- find two elems of nums which add up to target
twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum nums target
        = head $ do i <- nums
                    case binSearch sortedArr (target - i)
                      of Nothing -> []
                         Just ind -> return (i, sortedArr ! ind)
        where sortedArr = mkArr (length nums) nums

testTwoSum :: [([Int], Int)] -> [(([Int],Int), Maybe Int)]
testTwoSum = undefined

a = mkArr 10 [23,4,99,3,4,5,1,100,34,1,7,8]

main = do
        pArr a
        let b = msort a
        pArr b
        print $ binSearch b 23
        putStrLn "hello world"
        print $ twoSum [1,2,3] 4

