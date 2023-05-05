


-- We represent a spin as a single byte.  In principle, we need only
-- two values (-1 or 1), but Futhark represents booleans a full byte
-- entirely, so using an i8 instead takes no more space, and makes the
-- arithmetic simpler.
type spin = i8



def fst (a, _) = a
def snd (_, b) = b


import "lib/github.com/diku-dk/cpprandom/random"

-- Pick an RNG engine and define random distributions for specific types.
module rng_engine = minstd_rand
module rand_f32 = uniform_real_distribution f32 rng_engine
module rand_i8 = uniform_int_distribution i8 rng_engine

-- We can create an few RNG state with 'rng_engine.rng_from_seed [x]',
-- where 'x' is some seed.  We can split one RNG state into many with
-- 'rng_engine.split_rng'.
--
-- For an RNG state 'r', we can generate random integers that are
-- either 0 or 1 by calling 'rand_i8.rand (0i8, 1i8) r'.
--
-- For an RNG state 'r', we can generate random floats in the range
-- (0,1) by calling 'rand_f32.rand (0f32, 1f32) r'.
--
-- Remember to consult
-- https://futhark-lang.org/pkgs/github.com/diku-dk/cpprandom/latest/

def rand = rand_f32.rand (0f32, 1f32)


-- Create a new grid of a given size.  Also produce an identically
-- sized array of RNG states.
def random_grid (seed: i32) (h: i64) (w: i64) 
              : ([h][w]rng_engine.rng, [h][w]spin) =
  let myrng = rng_engine.rng_from_seed [seed]
  let vecDim = h * w
  let seeds = rng_engine.split_rng vecDim myrng
  let rndVec = map (rand_i8.rand (0i8, 1i8)) seeds
  let rndVecValues = map snd rndVec
  let rndVecValues2 = map (\x -> if x == 1 then 1 else -1) rndVecValues
  let matValues = unflatten h w rndVecValues2
  let rndVecSeeds = map fst rndVec
  let matSeeds = unflatten h w rndVecSeeds
  --let res = scan (\((seed, num), _) -> rand_i8.rand (0i8, 1i8) seed) (myrng, -1) numbers
  in (matSeeds, matValues)

-- Compute $\Delta_e$ for each spin in the grid, using wraparound at
-- the edges.
def deltas [h][w] (spins: [h][w]spin): [h][w]i8 =
  let mat = spins
  let len = h * w
  let matFlat = flatten mat :> [len]i8
  let dx = flatten (map (rotate 1) mat) :> [len]i8
  let sx = flatten (map (rotate (-1)) mat) :> [len]i8
  let down = flatten (rotate 1 mat) :> [len]i8
  let up = flatten (rotate (-1) mat) :> [len]i8
  let calcDelta el sx dx up down = 2 * el * (sx + dx + up + down)
  let comb = map5 calcDelta matFlat sx dx up down 
  in unflatten h w comb

-- The sum of all deltas of a grid.  The result is a measure of how
-- ordered the grid is.
def delta_sum [h][w] (spins: [w][h]spin): i32 =
  let spinFlatten = flatten spins
  let spinFlatten32 = map i32.i8 spinFlatten
  in reduce (+) (0:i32) spinFlatten32


-- let x = random_grid 1 5 5
-- let rngs = fst x
-- let spins = snd x
-- t 10 0.3 rngs spins



def exp (x) = (2.71828182846:f32) ** x   --todo how to use futhark's exp function?? importing math does not seem to solve :(

def magicFormula (t:f32) (p:f32) (c:i8) (deltaE:i8) (a:f32) (b:f32) = 
  let deltaEFloat = f32.i8 deltaE
  let right = exp (- deltaEFloat / t)
  in if (a < p) && ((deltaE < (-deltaE)) || (b < right)) then -c else c


-- Take one step in the Ising 2D simulation.
def step [h][w] (abs_temp: f32) (samplerate: f32)
                (rngs: [h][w]rng_engine.rng) (spins: [h][w]spin)
              : ([h][w]rng_engine.rng, [h][w]spin) =
  let dim = h * w
  let rngFlat = flatten rngs
  let aTemp = map (rand_f32.rand (0f32, 1f32)) rngFlat
  let a = map snd aTemp :> [dim]f32
  let bTemp = map (rand_f32.rand (0f32, 1f32)) (map fst aTemp)
  let b = map snd bTemp :> [dim]f32
  let rngOut = unflatten h w (map fst bTemp)
  let myDeltas = deltas spins
  let deltaFlat = flatten myDeltas :> [dim]i8
  let spinFlat = flatten spins :> [dim]i8
  let resFlatten = map4 (magicFormula abs_temp samplerate) spinFlat deltaFlat a b
  let res = unflatten h w resFlatten
  in (rngOut, res)

-- | Just for benchmarking.
def main (abs_temp: f32) (samplerate: f32)
         (h: i64) (w: i64) (n: i32): [h][w]spin =
  (loop (rngs, spins) = random_grid 1337 h w for _i < n do
     step abs_temp samplerate rngs spins).1

-- ==
-- entry: main
-- input { 0.5f32 0.1f32 10i64 10i64 2 } auto output

-- The following definitions are for the visualisation and need not be modified.

type~ state = {cells: [][](rng_engine.rng, spin)}

entry tui_init seed h w : state =
  let (rngs, spins) = random_grid seed h w
  in {cells=map (uncurry zip) (zip rngs spins)}

entry tui_render (s: state) = map (map (.1)) s.cells

entry tui_step (abs_temp: f32) (samplerate: f32) (s: state) : state =
  let rngs = (map (map (.0)) s.cells)
  let spins = map (map (.1)) s.cells
  let (rngs', spins') = step abs_temp samplerate rngs spins
  in {cells=map (uncurry zip) (zip rngs' spins')}
