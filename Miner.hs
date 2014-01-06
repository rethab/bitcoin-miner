{-# LANGUAGE OverloadedStrings #-}

module Miner ( Target
             , Data
             , Nonce
             , NonceBin
             , smaller
             , work) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString    as BS
import qualified HexDecoder         as Decoder

import Util                         ( chunksOf, encodeWord32le )

type Target = BS.ByteString
type Data = BS.ByteString
type Nonce = Integer
type NonceBin = BS.ByteString

{- Takes the maximum value of the Nonce,
 - the target string and the data string
 -}
work :: Nonce -> Target -> Data -> (Nonce, Maybe NonceBin)
work = go 0
    where go nonce maxNonce target dat
            | nonce > maxNonce = (nonce, Nothing)
            | isSolution hash && normalize hash `smaller` target
                = (nonce, Just nonceBin)
            | otherwise = go (nonce+1) maxNonce target dat
                where blk_hdr = BS.take 76 . bufreverse . Decoder.encode $ dat
                      hash = btcHash (blk_hdr `BS.append` nonceBin)
                      normalize = wordreverse . bufreverse
                      btcHash = SHA256.hash . SHA256.hash
                      nonceBin = encodeWord32le nonce
                      isSolution = BS.isSuffixOf (BS.pack [0,0,0,0])

{- Compares the elements of two bytestrings as if they were
 - numbers and decides which one is smaller
 -}
smaller :: BS.ByteString -> BS.ByteString -> Bool
smaller a b
    | BS.length a /= BS.length b = error "ByteStrings must be of equal length"
    | otherwise                  = decide . dropWhile (== EQ) $ BS.zipWith compare a b
    where -- all equal elems are gone
          decide :: [Ordering] -> Bool
          decide []     = False -- all were equal
          decide (LT:_) = True  -- first non-equal is less. bs is smaller
          decide (GT:_) = False -- first non-equal is greater. bs is greater

{- Takes chunks of four and bytereverses them
 -   (e.g. "abcdefgh" --> "dcbahgfe"0
 -}
bufreverse :: BS.ByteString -> BS.ByteString
bufreverse = go BS.empty
    where go acc bs
            | BS.null bs = acc
            | otherwise  = go (BS.append acc (BS.reverse h)) t
                where (h, t) = BS.splitAt 4 bs

{- Takes chunks of four and reverses their order
 -   (e.g. "abcdefgh" --> "efghabcd")
 -}
wordreverse :: BS.ByteString -> BS.ByteString
wordreverse = BS.pack . concat . reverse . chunksOf 4 . BS.unpack

