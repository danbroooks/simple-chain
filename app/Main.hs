module Main where

import SimpleChain

main :: IO ()
main = chain >>= printChain

chain :: IO (Blockchain Int)
chain =
  createChain
  >>= writeChain 7
  >>= writeChain 12
