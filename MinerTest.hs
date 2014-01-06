{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import HexDecoder           (bsToInt)
import Miner
import Control.Monad        (liftM2)
import Data.Char            (ord)
import Test.HUnit
import Test.QuickCheck.All
import Test.QuickCheck

import qualified Data.ByteString as BS

main = $quickCheckAll

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack `fmap` evenLengthList (elements hexOrds)
        where hexOrds = Prelude.map (fromIntegral . ord) hexChars
              hexChars = "0123456789abcdef"
              evenLengthList = flip suchThat (even . length) . listOf

{- Pair of ByteStrings with equal length. Compact notation -}
newtype EqLenBSPair = EqLenBSPair (BS.ByteString, BS.ByteString)
    deriving (Show)

instance Arbitrary EqLenBSPair where
    arbitrary = do bs1 <- BS.pack `fmap` arbitrary
                   bs2 <- BS.pack `fmap` vectorOf (BS.length bs1) arbitrary
                   return $ EqLenBSPair (bs1, bs2)

targetstr :: BS.ByteString
targetstr = "0000000000000000000000000000000000000000000000000000ffff00000000"

datastr :: BS.ByteString
datastr = "000000023aae877d5e1f94b9d9b28aef9b524e2fb41d7c04cc0b59ec000000010000000087ee8b15fd274b853129938c4ec7203abfe0b68cb60601306acd582812ae81b252b56c491903a30c00000000000000800000000000000000000000000000000000000000000000000000000000000000000000000000000080020000"

prop_smaller_eq_int_smaller (EqLenBSPair (bs1,bs2)) =
    not (BS.null bs1) && not (BS.null bs2) ==>
        (bs1 `smaller` bs2) == (bsToInt bs1 < bsToInt bs2)
