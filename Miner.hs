{-# LANGUAGE OverloadedStrings #-}

module Miner ( Target
             , Data
             , Nonce
             , NonceBin
             , work) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString    as BS
import qualified HexDecoder         as Decoder

import Util                         ( chunksOf, encodeWord32le )

type Target = Integer
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
            | isSolution hash && hashToInt hash < target
                = (nonce, Just nonceBin)
            | otherwise = go (nonce+1) maxNonce target dat
                where blk_hdr = BS.take 76 . bufreverse . Decoder.encode $ dat
                      hash = btcHash (blk_hdr `BS.append` nonceBin)
                      hashToInt = Decoder.bsToInt . wordreverse . bufreverse
                      btcHash = SHA256.hash . SHA256.hash
                      nonceBin = encodeWord32le nonce
                      isSolution = BS.isSuffixOf (BS.pack [0,0,0,0])


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

