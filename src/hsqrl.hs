{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Crypto.Scrypt
import Crypto.Random
--import Data.Maybe
--import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import Control.Monad
import qualified Data.Binary.Strict.BitGet as BG
import Data.Either.Unwrap

main :: IO ()
main = do
        mk <- genMasterKey 66
        putStrLn $ "key is " ++ show (BS.length mk) ++ " bytes long."
        let bits = bsToBl 521 mk
        putStrLn $ show $ length bits

genMasterKey :: Int -> IO BS.ByteString
--genMasterKey sz = do
genMasterKey sz = do
        pool <- createEntropyPool
        let cprg = cprgCreate pool :: SystemRNG
        let (b,_) = cprgGenerate sz cprg
        return b

bsToBl :: Int -> BS.ByteString -> [Bool]
bsToBl n bs = fromRight $ BG.runBitGet bs (replicateM n BG.getBit)
