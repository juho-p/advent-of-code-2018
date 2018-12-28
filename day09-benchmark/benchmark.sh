#!/bin/bash

for x in *.hs
do
    ghc -Odph $x
done

rustc -O *.rs

for x in haskell-seq haskell-mut rust-deque
do
    echo $x
    time for ((i=0;i<10;i++)); do ./$x >/dev/null; done
done
