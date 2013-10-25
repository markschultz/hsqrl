{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Crypto.Scrypt
import Crypto.Random
--import Data.Maybe
import Data.ByteString

main :: IO ()
main = error "error"

genMasterKey' sz = fst $ cprgGenerate sz cprg
    where
        cprg = createEntropyPool >>= \pool -> cprgCreate pool
