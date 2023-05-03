

def fact (n: i32): i32 = reduce (*) 1 (1...n)



-- futhark dataset -b --i32-bounds=-10000:10000 -g [100]i32 -g [100]i32 > dataset100
-- futhark dataset -b --i32-bounds=-10000:10000 -g [1000]i32 -g [1000]i32 > dataset1000
-- futhark dataset -b --i32-bounds=-10000:10000 -g [10000]i32 -g [10000]i32 > dataset10000
-- futhark dataset -b --i32-bounds=-10000:10000 -g [100000]i32 -g [100000]i32 > dataset100000
-- futhark dataset -b --i32-bounds=-10000:10000 -g [1000000]i32 -g [1000000]i32 > dataset1000000
-- futhark dataset -b --i32-bounds=-10000:10000 -g [5000000]i32 -g [5000000]i32 > dataset5000000
-- futhark dataset -b --i32-bounds=-10000:10000 -g [10000000]i32 -g [10000000]i32 > dataset10000000


-- Test the process function.
-- ==
-- entry: process
-- input { [23,45,-23,44,23,54,23,12,34,54,7,2, 4,67] [-2, 3, 4,57,34, 2, 5,56,56, 3,3,5,77,89] } output { 73 }
-- input @ dataset100
-- input @ dataset1000
-- input @ dataset10000
-- input @ dataset100000
-- input @ dataset1000000
-- input @ dataset5000000
-- input @ dataset10000000

--val process [n] : (xs: [n]i32) -> (ys: [n]i32) -> i32

entry process (a: []i32) (b: []i32): i32 = 
    let differences = map2 (-) a b
    let differencesAbs = map i32.abs differences
    let maxDifference = reduce i32.max 0 differencesAbs
    in maxDifference


def main (a: []i32) (b: []i32) = process a b

