module IMP
    ( INS(..),
    encode,
    encodeAll,
    toInts
    ) where

import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.List as L

data INS =
    JZ Int |
    JMP Int |
    CAL Int |
    MRK Int |
    LD Int |
    ST Int |
    LDR Int |
    STR Int |
    IMM Int |
    SHL Int |
    SHR Int |
    ADD |
    AND |
    OR |
    GE 
    deriving (Show, Read)

toBits :: Int -> Int -> [Bool]
toBits len n = map (testBit n) [len - 1,len - 2..0]

encode :: INS -> [Bool]
encode (JZ i) = [False, False, False] ++ toBits 8 i
encode (JMP i) = [False, False, True] ++ toBits 8 i
encode (CAL i) = [False, True, False] ++ toBits 8 i
encode (MRK i) = [False, True, True] ++ toBits 8 i

encode (LD i) = [True, False, False, False] ++ toBits 7 i
encode (ST i) = [True, False, False, True] ++ toBits 7 i
encode (LDR i) = [True, False, True, False] ++ toBits 7 i
encode (STR i) = [True, False, True, True] ++ toBits 7 i

encode (IMM i) = [True, True, False] ++ toBits 8 i

encode (SHL i) = [True, True, True, False, False] ++ toBits 6 i
encode (SHR i) = [True, True, True, False, True] ++ toBits 6 i

encode ADD = [True, True, True, True, False, False]
encode AND = [True, True, True, True, False, True]
encode OR  = [True, True, True, True, True, False]
encode GE  = [True, True, True, True, True, True]

encodeAll :: [INS] -> [Bool]
encodeAll = concatMap encode

toInt :: [Bool] -> Word8 -> Word8
toInt [] a = a
toInt (x:xs) v = toInt xs (v * 2 + if x then 1 else 0)

toInts' :: [[Bool]] -> [Word8] -> [Word8]
toInts' [] acc = acc
toInts' (x:[]) acc = toInt (x ++ replicate (8 - length x) False) 0 : acc
toInts' (x:xs) acc = toInts' xs ((toInt x 0) : acc)

toInts :: [Bool] -> [Word8]
toInts = reverse . flip toInts' [] . L.unfoldr (\ a -> if null a then Nothing else Just (take 8 a, drop 8 a))
