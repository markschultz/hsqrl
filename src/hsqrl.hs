
module Main where

import Crypto.Scrypt
import Crypto.Random
import Crypto.Hash
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
--import Data.ByteString.Lazy.Internal
import Data.ByteString.Lazy (toStrict)
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.Binary.BitPut as BP
import Data.Either.Unwrap
import Data.Digits
import Data.Byteable
import Control.Monad
--import Data.Bits

main :: IO ()
main = do
        --let keyLen = length $ digits 2 n521
        --let pad = 8 - (keyLen `mod` 8)
        --let bytes = (keyLen + pad + 64) `div` 8
        --let bytes = (keyLen + pad) `div` 8
        mk <- genRandomBS keyBytes
        salt <- genRandomBS keyBytes
        let epass = scrypt (fromJust $ scryptParamsLen 18 8 1 (toInteger keyBytes)) (Salt salt) (Pass pw)
        let bools = bsToBl (BS.length mk * 8) mk
        let boolspw = bsToBl ((BS.length $ getHash epass) * 8) (getHash epass)
        let mixed = zipWith xor'' bools boolspw
        let mixedBs = blToBs (length mixed) $ unDigits 2 (btoi mixed)
        let h = toBytes $ hmacAlg SHA512 mixedBs url
        putStrLn $ show $ BS.length h
        --let c = unDigits 10 $ convertBase 2 10 (btoi $ bools)
        --let d = (c `mod` (n521 - 1)) + 1
        --putStrLn $ "d bitlength = " ++ show (length $ digits 2 d)
        --putStrLn $ "c > n - 2 : " ++ show (c > (n521 - 2))
        --putStrLn $ show $ foldli (\z acc x -> z + (2 ^ acc)*x) 0 (length bools - 1) (btoi $ bools)
        putStrLn "Done."

genRandomBS :: Int -> IO BS.ByteString
genRandomBS sz = do
        pool <- createEntropyPool
        let cprg = cprgCreate pool :: SystemRNG
        let (b,_) = cprgGenerate sz cprg
        return b

bsToBl :: Int -> BS.ByteString -> [Bool]
bsToBl n bs = fromRight $ BG.runBitGet bs (replicateM n BG.getBit)

blToBs :: Int -> Integer -> BS.ByteString
blToBs n int = toStrict $ BP.runBitPut $ BP.putNBits n int

btoi :: [Bool] -> [Integer]
btoi [] = []
btoi (x:xs) = (if x == True then [1] :: [Integer] else [0] :: [Integer]) ++ btoi xs

foldli :: Num a => (t -> a -> t1 -> t) -> t -> a -> [t1] -> t
foldli f z acc (x:xs) = foldli f (f z acc x) (acc-1) xs
foldli _ z _ [] = z

convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from

xor'' :: Bool -> Bool -> Bool
xor'' True False = True
xor'' False True = True
xor'' _ _ = False

xor' :: Integer -> Integer -> Integer
xor' 1 0 = 1
xor' 0 1 = 1
xor' _ _ = 0

xorInt :: Integer -> Integer -> Integer
xorInt x y = unDigits 2 $ zipWith xor' paddedx paddedy
    where
        padTo = if lenx > leny then lenx else leny
        xs = digits 2 x
        ys = digits 2 y
        lenx = length xs
        leny = length ys
        padx = padTo - lenx
        pady = padTo - leny
        paddedx = replicate padx 0 ++ xs
        paddedy = replicate pady 0 ++ ys

n521 :: Integer
n521 = 6864797660130609714981900799081393217269435300143305409394463459185543183397655394245057746333217197532963996371363321113864768612440380340372808892707005449

pw :: BS.ByteString
pw = C8.pack "test"

keyBytes :: Int
keyBytes = 64

url :: BS.ByteString
url = C8.pack "example.com"

