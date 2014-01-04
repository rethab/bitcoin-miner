{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative  ( (<$>), (<*>) )
import Control.DeepSeq      ( deepseq )
import Data.Aeson           ( FromJSON
                            , ToJSON
                            , Value(..)
                            , toJSON
                            , parseJSON
                            , encode
                            , decode
                            , object
                            , (.:)
                            , (.=) )
import Data.ByteString.Lazy ( fromStrict
                            , toStrict )
import Data.Foldable        ( forM_ )
import Data.Maybe           ( fromJust
                            , isJust )
import Network.HTTP         ( RequestMethod(POST)
                            , Request(Request)
                            , Header(Header)
                            , HeaderName(..)
                            , simpleHTTP
                            , rspBody )
import Network.URI          ( parseURI )
import System.Time          ( diffClockTimes
                            , getClockTime
                            , tdSec )

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as Char8
import qualified HexDecoder             as Decoder

import Miner      ( Data, Nonce, NonceBin, Target, work )
import HexDecoder ( bsToInt )

host :: String
host = "http://api.bitcoin.cz:8332"

auth :: String
auth = "Basic " ++ Char8.unpack (Base64.encode "rethab.worker1:MDFgJzeo")

scantime :: Int
scantime = 30

defaultMaxNonce :: Nonce
defaultMaxNonce = 250000

type OBJID = Integer

data  BTCRequest = BTCRequest {
    reqVersion :: String
  , reqMethod  :: String
  , reqId      :: OBJID
  } deriving (Show)

data BTCResponse = BTCResponse {
    respError  :: Maybe String
  , respId     :: Int -- what am i / datatype?
  , respResult :: BTCResult
  } deriving (Show)

data BTCResult = BTCResult {
    resHash     :: BS.ByteString -- datatype?
  , resData     :: Data
  , resTarget   :: BS.ByteString
  , resMidstate :: BS.ByteString
  } deriving (Show)
 
instance ToJSON BTCRequest where
    toJSON (BTCRequest v m i) = object [ "version" .= v
                                       , "method" .= m
                                       , "id" .= i
                                       ]

instance FromJSON BTCResponse where
    parseJSON (Object v) = BTCResponse   <$>
                           v .: "error"  <*>
                           v .: "id"     <*>
                           v .: "result"

instance FromJSON BTCResult where
    parseJSON (Object v) = BTCResult       <$>
                           v .: "hash1"    <*>
                           v .: "data"     <*>
                           v .: "target"   <*>
                           v .: "midstate"

main :: IO ()
main = mine defaultMaxNonce

mine :: Nonce -> IO ()
mine maxNonce = do (target, dat) <- getWork
                   start <- getClockTime
                   let res@(nhashes, mbnonce) = work maxNonce target dat
                   end <- deepseq res getClockTime
                   let time = tdSec (diffClockTimes end start)
                       maxNonce' = nextMaxNonce nhashes time 
                   printStats time nhashes (isJust mbnonce)
                   forM_ mbnonce (submitWork dat)
                   mine maxNonce'
    where nextMaxNonce nhashes time =
            (nhashes * fromIntegral scantime) `div` fromIntegral time

getWork :: IO (Target, Data)
getWork = do eres <- simpleHTTP (req 1)
             let bs = either (error . show) rspBody eres
             let res = respResult (fromJust . decode . fromStrict $ bs)
             return (intTarget res, resData res)
    where intTarget = targetToInt . resTarget

submitWork :: Data -> NonceBin -> IO ()
submitWork = error "submitwork"

printStats :: Int -> Nonce -> Bool -> IO ()
printStats time nhashes success =
    do putStrLn ("Time: " ++ show time ++ "s")
       putStrLn ("Hashes: " ++ show nhashes)
       putStrLn ("Khash/sec: " ++ show khashes)
       putStrLn ("Success: " ++ show success)
    where khashes = (fromIntegral nhashes `div` 1000) `div` time

targetToInt :: BS.ByteString -> Integer
targetToInt = bsToInt . BS.reverse . Decoder.encode
          
req :: OBJID -> Request BS.ByteString
req objid = Request uri POST headers body
    where uri = fromJust (parseURI host)
          headers = [ Header HdrContentType "application/json"
                    , Header HdrContentLength (show $ BS.length body)
                    , Header HdrConnection ""
                    , Header HdrAuthorization auth
                    ]
          body = toStrict . encode $ BTCRequest "1.1" "getwork" objid
