{-# LANGUAGE TemplateHaskell #-}

module Main where

import HexDecoder
import Data.Char            (ord)
import Test.HUnit
import Test.QuickCheck.All
import Test.QuickCheck

import qualified Data.ByteString as BS

main = $quickCheckAll >> runTestTT htests

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack `fmap` evenLengthList (elements hexOrds)
        where hexOrds = Prelude.map (fromIntegral . ord) hexChars
              hexChars = "0123456789abcdef"
              evenLengthList = flip suchThat (even . length) . listOf

prop_half_length bs =
    (fromIntegral (BS.length bs) `div` 2) == BS.length (encode bs)

prop_reverse_function_e_d bs =
    decode (encode bs) == bs

prop_reverse_function_d_e bs =
    encode (decode bs) == bs

htests = TestList [ hexToDigitTest, digitToHexTest ]

hexToDigitTest = TestList 
     [  1 ~=? hexToDigit (w_ord '1')
     ,  2 ~=? hexToDigit (w_ord '2')
     ,  3 ~=? hexToDigit (w_ord '3')
     ,  4 ~=? hexToDigit (w_ord '4')
     ,  5 ~=? hexToDigit (w_ord '5')
     ,  6 ~=? hexToDigit (w_ord '6')
     ,  7 ~=? hexToDigit (w_ord '7')
     ,  8 ~=? hexToDigit (w_ord '8')
     ,  9 ~=? hexToDigit (w_ord '9')
     , 10 ~=? hexToDigit (w_ord 'a')
     , 10 ~=? hexToDigit (w_ord 'A')
     , 11 ~=? hexToDigit (w_ord 'b')
     , 11 ~=? hexToDigit (w_ord 'B')
     , 12 ~=? hexToDigit (w_ord 'c')
     , 12 ~=? hexToDigit (w_ord 'C')
     , 13 ~=? hexToDigit (w_ord 'd')
     , 13 ~=? hexToDigit (w_ord 'D')
     , 14 ~=? hexToDigit (w_ord 'e')
     , 14 ~=? hexToDigit (w_ord 'E')
     , 15 ~=? hexToDigit (w_ord 'f')
     , 15 ~=? hexToDigit (w_ord 'F')
     ]
 where w_ord = fromIntegral . ord

digitToHexTest = TestList 
     [ digitToHex 1  ~?= w_ord '1'
     , digitToHex 2  ~?= w_ord '2'
     , digitToHex 3  ~?= w_ord '3'
     , digitToHex 4  ~?= w_ord '4'
     , digitToHex 5  ~?= w_ord '5'
     , digitToHex 6  ~?= w_ord '6'
     , digitToHex 7  ~?= w_ord '7'
     , digitToHex 8  ~?= w_ord '8'
     , digitToHex 9  ~?= w_ord '9'
     , digitToHex 10 ~?= w_ord 'a'
     , digitToHex 11 ~?= w_ord 'b'
     , digitToHex 12 ~?= w_ord 'c'
     , digitToHex 13 ~?= w_ord 'd'
     , digitToHex 14 ~?= w_ord 'e'
     , digitToHex 15 ~?= w_ord 'f'
     ]
 where w_ord = fromIntegral . ord
