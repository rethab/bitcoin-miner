module HexDecoder ( decode
                  , digitToHex
                  , encode
                  , hexToDigit
                  , separate
                  ) where

import Data.Bits  ( (.|.), (.&.), shiftL, shiftR )
import Data.Char  ( ord )
import Data.Maybe ( fromJust )
import Data.Word  ( Word8 )

import qualified Data.ByteString as BS


{- Encodes each two elements of a string into one.
 -    (e.g. ['f', 'f'] --> [255]
 -}
encode :: BS.ByteString -> BS.ByteString
encode = BS.pack . go . BS.unpack
    where go [] = []
          go (a:b:xs) = combine a b : go xs

{- Takes two 4 bit values and puts them into one 8 bit value
 -   (e.g. 0010 -> 1100 --> 00101100)
 -}
combine :: Word8 -> Word8 -> Word8
combine h l = shiftL (hexToDigit h) 4 .|. hexToDigit l

{- converts an index in the asci table to its numeric value
 -  (e.g. 'f' -> 15)
 -}
hexToDigit :: Word8 -> Word8
hexToDigit h | h >= zero && h <= nine = h - zero
             | h >= lowa && h <= lowf = h - lowa + 10
             | h >= higa && h <= higf = h - higa + 10
             | otherwise              = error ("Not a Hex Char " ++ show h)
    where zero = fromIntegral (ord '0')
          nine = fromIntegral (ord '9')
          lowa = fromIntegral (ord 'a')
          lowf = fromIntegral (ord 'f')
          higa = fromIntegral (ord 'A')
          higf = fromIntegral (ord 'F')

{- Converts a numeric value to its index in the ascii tables
 -   (e.g. 15 to 'f')
 -}
digitToHex :: Word8 -> Word8
digitToHex d | d < 10    = d + zero
             | otherwise = d - 10 + lowa
    where zero = fromIntegral (ord '0')
          lowa = fromIntegral (ord 'a')

{- Decodes each element into two
 -    (e.g. [255] --> ['f', 'f']
 -}
decode :: BS.ByteString -> BS.ByteString
decode = go BS.empty
    where go :: BS.ByteString -> BS.ByteString -> BS.ByteString
          go acc bs 
            | BS.null bs = acc
            | otherwise  = go (acc `BS.append` unHex h) t
                where (h, t) = fromJust $ BS.uncons bs
                      unHex = BS.pack . map digitToHex . separate

separate :: Word8 -> [Word8]
separate w = [ shiftR w 4 , w .&. 0xF ]
