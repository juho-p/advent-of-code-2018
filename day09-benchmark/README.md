# AOC 2018 day 9 benchmark

## Implementations

Each implementation use the same algorithm, but different data structures

* https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Sequence.html
* https://hackage.haskell.org/package/mutable-containers-0.3.3/docs/Data-Mutable.html
* https://doc.rust-lang.org/std/collections/struct.VecDeque.html

Source code can be found in this directory.

## Results

Benchmarking was done on laptop with Intel i5-5200U CPU. Average time for ten
runs (both part 1 and part 2) for each implementation:

* Haskell (immutable Seq): 2.29s
* Haskell (mutable Deque): 1.11s
* Rust (mutable Deque): 0.100s

Conclusion: Rust is fast, but Haskell Data.Sequence is not that bad compared to
mutable deque.
