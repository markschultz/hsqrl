{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Crypto.Scrypt
import Crypto.Random
--import Data.Maybe
import qualified Data.ByteString as BS
import Control.Monad
import qualified Data.Binary.Strict.BitGet as BG
import Data.Either.Unwrap
import Data.Digits

n521 :: Integer
n521 = 6864797660130609714981900799081393217269435300143305409394463459185543183397655394245057746333217197532963996371363321113864768612440380340372808892707005449

main :: IO ()
main = do
        let keyLen = length $ digits 2 n521
        let pad = 8 - (keyLen `mod` 8)
        --let genLen = ((keyLen + 64) / 8) :: Int
        mk <- genMasterKey ((keyLen + pad + 64) `div` 8)
        putStrLn $ "key is " ++ show (BS.length mk) ++ " bytes long."
        --putStrLn $ show $ B6.encode $ trimLeft 7 mk
        let bools = bsToBl (BS.length mk * 8) mk
        putStrLn $ show pad ++ " = pad length"
        --let bools2 = drop 13 $ bsToBl ((BS.length mk * 8) ) $ trimLeft 7 mk
        --let bools3 = drop 7 $ bsToBl ((BS.length mk * 8) ) mk
        --putStrLn $ show $ btoi $ bools
        --putStrLn $ show $ btoi $ bools2
        --putStrLn $ show $ btoi $ bools3
        --putStrLn "-------foldli-------"
        --putStrLn $ show $ foldli (\z acc x -> z + (2 ^ acc)*x) 0 (length bools - 1) (btoi $ bools)
        --putStrLn "-------convert------"
        --putStrLn $ show $ foldl (\a b -> b + (10*a)) (0 :: Integer) (convertBase 2 10 (btoi $ bools))
        

genMasterKey :: Int -> IO BS.ByteString
genMasterKey sz = do
        pool <- createEntropyPool
        let cprg = cprgCreate pool :: SystemRNG
        let (b,_) = cprgGenerate sz cprg
        return b

bsToBl :: Int -> BS.ByteString -> [Bool]
bsToBl n bs = fromRight $ BG.runBitGet bs (replicateM n BG.getBit)

btoi :: [Bool] -> [Integer]
btoi [] = []
btoi (x:xs) = (if x == True then [1] :: [Integer] else [0] :: [Integer]) ++ btoi xs

foldli :: Num a => (t -> a -> t1 -> t) -> t -> a -> [t1] -> t
foldli f z acc (x:xs) = foldli f (f z acc x) (acc-1) xs
foldli _ z _ [] = z

convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from
