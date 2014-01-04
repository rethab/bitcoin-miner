module Util ( chunksOf, encodeWord32le) where

import Data.Binary.Put ( runPut, putWord32le )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

{- Splits the list into sub-lists of the specified size
 -  (e.g. chunksOf 2 "abcd" --> ["ab", "cd"])
 -}
chunksOf :: Int -> [a] -> [[a]]
chunksOf n (a:b:c:d:xs) = [a,b,c,d] : chunksOf n xs
chunksOf _ x            = [x]

encodeWord32le :: (Integral a) => a -> BS.ByteString
encodeWord32le = LBS.toStrict . runPut . putWord32le . fromIntegral
