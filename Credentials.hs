{-# LANGUAGE OverloadedStrings #-}
module Credentials (auth) where

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as Char8

auth :: String
auth = "Basic " ++ Char8.unpack (Base64.encode "username:password")
