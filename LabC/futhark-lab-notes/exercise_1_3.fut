-- ==
-- input @ two_100_i32s
-- input @ two_1000_i32s
-- input @ two_10000_i32s
-- input @ two_100000_i32s
-- input @ two_1000000_i32s
-- input @ two_5000000_i32s
-- input @ two_10000000_i32s

def process_idx [n] (xs: [n]i32) (ys: [n]i32): (i32,i64) =
  let max (d1,i1) (d2,i2) =
        if      d1 > d2 then (d1,i1)
        else if d2 > d1 then (d2,i2)
        else if i1 > i2 then (d1,i1)
        else                 (d2,i2)
  in reduce_comm max (0, -1)
                 (zip (map i32.abs (map2 (-) xs ys))
                      (iota n))

def main (xs: []i32) (ys: []i32) =
  process_idx xs ys
