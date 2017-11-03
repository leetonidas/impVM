module Main where

import System.Environment
import qualified Data.ByteString as BS

import IMP

main :: IO ()
main = (\ a -> BS.writeFile (a !! 1) =<< (BS.pack . toInts . concatMap (encode . read) . filter (not . null) . lines) <$> readFile (a !! 0)) =<< getArgs
