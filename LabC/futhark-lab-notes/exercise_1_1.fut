-- ==
-- input @ two_100_i32s
-- input @ two_1000_i32s
-- input @ two_10000_i32s
-- input @ two_100000_i32s
-- input @ two_1000000_i32s
-- input @ two_5000000_i32s
-- input @ two_10000000_i32s

def process (xs: []i32) (ys: []i32): i32 =
  reduce i32.max 0 (map i32.abs (map2 (-) xs ys))

def main (xs: []i32) (ys: []i32) =
  process xs ys
