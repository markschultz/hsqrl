module Hecc where
-- borrowed from:
-- https://github.com/singpolyma/ripple-haskell/blob/master/Hecc.hs

import Data.Word
import Data.Sequence (unfoldl)
import Data.Foldable (toList)

import Crypto.Types.PubKey.ECC (Curve(CurveFP), CurvePrime(..), CurveCommon(..), Point(..))
import Codec.Crypto.ECC.Base

-- Adapters for hecc

curve2hecc :: Curve -> EC Integer
curve2hecc (CurveFP (CurvePrime p (CurveCommon a b _ n 1))) =
        ECi (8 * length (unroll n)) a b p n
curve2hecc _ = error "TODO: binary curves"

point2hecc :: Curve -> Point -> ECPF Integer
point2hecc curve (Point x y) =
        ECPa (curve2hecc curve) x y

hecc2point :: ECPF Integer -> Point
hecc2point p = Point (getx p) (gety p)

toBytesCompressed :: ECPF Integer -> [Word8]
toBytesCompressed point =
        (if gety point `mod` 2 == 0 then 0x02 else 0x03) :
        unroll (getx point)

unroll :: Integer -> [Word8]
unroll = toBase 256

toBase :: (Integral a, Integral b) => a -> a -> [b]
toBase _ 0 = [0]
toBase b v
    | v < 0 = error "toBase v < 0"
    | otherwise = map fromIntegral $ toList $
        unfoldl (\n -> if n == 0 then Nothing else Just $! (n `divMod` b)) v
