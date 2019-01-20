# AOC 2018 day 9 benchmark

## Implementations

Each implementation use similar algorithm, but different data structure

* https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Sequence.html
* https://hackage.haskell.org/package/pointedlist-0.6.1/docs/Data-List-PointedList.html
* https://hackage.haskell.org/package/mutable-containers-0.3.3/docs/Data-Mutable.html
* https://doc.rust-lang.org/std/collections/struct.VecDeque.html

Source code can be found in this directory.

## Results

Benchmarking was done on laptop with Intel i7-4770 CPU. Average time for ten
runs (both part 1 and part 2) for each implementation:

* Haskell (immutable Seq): 1.54s
* Haskell (pointed list): 1.35s
* Haskell (mutable Deque): 0.726s
* Rust (mutable Deque): 0.0670s
