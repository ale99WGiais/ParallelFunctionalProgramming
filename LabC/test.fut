

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


-- Test the process function.
-- ==
-- entry: process2
-- input { [23,45,-23,44,23,54,23,12,34,54,7,2, 4,67] [-2, 3, 4,57,34, 2, 5,56,56, 3,3,5,77,89] }
-- input @ dataset100
-- input @ dataset1000
-- input @ dataset10000
-- input @ dataset100000
-- input @ dataset1000000
-- input @ dataset5000000
-- input @ dataset10000000
entry process2 [n] (a: [n]i32) (b: [n]i32) : (i32, i64) = 
    let differences = map2 (-) a b
    let differencesAbs = map i32.abs differences
    let differencesAbsWithPos = map2 (\x y -> (x, y)) (differencesAbs) (iota n)
    --let maxDifference = reduce (\(min, posMin) (newX, )) (0, 0) differencesAbs
    in reduce 
        (\(vMax, pMax) (vNew, pNew) -> (
            i32.max vMax vNew, 
            if vMax < vNew then pNew else if vMax > vNew then pMax else (i64.min pMax pNew)
        )) 
        (0, 0) 
        differencesAbsWithPos


def main (a: []i32) (b: []i32) = process a b


def fst (a, _) = a
def snd (_, b) = b


def segscan [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)) :[n]t= 
    let res = scan (\(vAcc, fAcc) (vNew, fNew) -> (if fNew then vNew else (op vAcc vNew), fAcc || fNew)) (ne, false) arr
    in map fst res


def rotateLeftAndAppend [n] 't (v: [n]t) (end: t) :[n]t = 
    tabulate n (\i -> if i == n - 1 then end else v[i + 1]) 


def segreduce [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)) = 
    let segscanRes = segscan op ne arr
    let ints = map i64.bool (map snd arr)
    let sums = scan (+) 0 ints
    let shifted = rotateLeftAndAppend sums (n*2)
    let takenPos = map2 (\a b -> (b > a) && (a > 0)) sums shifted
    let res = filter snd (zip segscanRes takenPos)
    in map fst res

-- segreduce (+) 0 [(1, false), (2, true), (3, false), (4, false), (5, true), (6, false), (7, true), (8, false), (9, true)] 

-- segscan (+) 0 [(1, true), (2, false), (2, false),(2, false), (3, true),(2, false),(1, false)] 
-- segreduce (+) 0 [(1, true), (2, false), (2, false),(2, false), (3, true),(2, false),(1, false)] 


import "lib/github.com/diku-dk/sorts/radix_sort"



def rotateRightAndAppend [n] 't (v: [n]t) (end: t) :[n]t = 
    tabulate n (\i -> if i == 0 then end else v[i - 1]) 


def hist 'a [n] (op : a -> a -> a) (ne : a) (k: i64) (iss : [n]i64) (ass : [n]a) = 
    let sz = n + k
    let is2 = concat iss (iota k) :> [sz]i64
    let as2 = concat ass (replicate k ne) :> [sz]a
    let sorted = radix_sort_by_key fst i64.num_bits i64.get_bit (zip is2 as2)
    let is2Sorted = map fst sorted
    let as2Sorted = map snd sorted
    let isShifted = rotateRightAndAppend is2Sorted (-1)
    let begins = map2 (\a b -> b < a) is2Sorted isShifted
    let res = segreduce op ne (zip as2Sorted begins)
    in res
    
-- def t = radix_sort_by_key fst i64.num_bits i64.get_bit [(1, "a"), (0, "b")]

-- hist (+) 0 5 [0, 0, 0, 1, 1, 2, 2, 2, 2, 3, 3] [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]



-- size coercion
-- def concat_to 'a (m: i32) (a: []a) (b: []a) : [m]a =
--   a ++ b :> [m]a