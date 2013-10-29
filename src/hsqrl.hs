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

main :: IO ()
main = do
        mk <- genMasterKey 66
        putStrLn $ "key is " ++ show (BS.length mk) ++ " bytes long."
        --putStrLn $ show $ B6.encode mk
        --putStrLn $ show $ B6.encode $ trimLeft 7 mk
        let bools = bsToBl (BS.length mk * 8) mk
        --putStrLn $ show $ take 8 bools
        putStrLn $ show $ btoi $ take 8 bools
        putStrLn $ show $ foldli (\z acc x -> z + (2 ^ acc)*x) 0 (8-1) (btoi $ take 8 bools)
        putStrLn "--------------"
        putStrLn $ show $ btoi $ bools
        putStrLn $ show $ foldli (\z acc x -> z + (2 ^ acc)*x) 0 (length bools - 1) (btoi $ bools)
        putStrLn $ show $ convertBase 2 10 (btoi $ bools)

genMasterKey :: Int -> IO BS.ByteString
genMasterKey sz = do
        pool <- createEntropyPool
        let cprg = cprgCreate pool :: SystemRNG
        let (b,_) = cprgGenerate sz cprg
        return b

bsToBl :: Int -> BS.ByteString -> [Bool]
bsToBl n bs = fromRight $ BG.runBitGet bs (replicateM n BG.getBit)

-- this pads left with false
trimLeft :: Int -> BS.ByteString -> BS.ByteString
trimLeft pad bs = fromRight $ BG.runBitGet bs (BG.getRightByteString ((BS.length bs * 8) - pad) )

--btoi' (x:xs) ni sum = sum + ((2 ^ ni) * (if x == True then 1 else 0)) 
btoi :: [Bool] -> [Integer]
btoi [] = []
btoi (x:xs) = (if x == True then [1] :: [Integer] else [0] :: [Integer]) ++ btoi xs

foldli :: Num a => (t -> a -> t1 -> t) -> t -> a -> [t1] -> t
foldli f z acc (x:xs) = foldli f (f z acc x) (acc-1) xs
foldli _ z _ [] = z

convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from
