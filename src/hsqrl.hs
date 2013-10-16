{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypto.Scrypt
--import Crypto.Random
--import Data.Digest.Pure.SHA
import Data.ByteString as B
import Data.Maybe

main :: IO ()
main = error "error"

genMasterKey :: Integer -> Integer -> Integer -> ByteString -> ByteString -> ByteString
genMasterKey logN r p sa pa = unHash $ scrypt params salt pass
    where
        params = fromJust $ scryptParams logN r p
        pass = Pass pa
        salt = Salt sa
