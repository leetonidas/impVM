module IMP
    ( INSHL(..),
    encode,
    encodeAll,
    toInts,
    toIR,
    inlineAdd,
    buildInAdd,
    Prog(..)
    ) where

import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.List as L

data Prog = Prog {
    funs :: [[INSHL]],
    dat :: [Int],
    datMLen :: Int
} deriving Show

data INSHL = 
    JZ Int |
    JMP Int |
    CAL Int |
    MRK Int |
    LD Int |
    LDC Int |
    ST Int |
    STC Int |
    LDR Int |
    LDRC Int |
    STR Int |
    STRC Int |
    IMM Int |
    SHL Int |
    SHR Int |
    POP |
    ADD |
    SUB |
    NOT |
    AND |
    OR |
    XOR |
    GE |
    EQ
    deriving (Show, Read, Eq)

data INS =
    I_JZ Int |
    I_JMP Int |
    I_CAL Int |
    I_MRK Int |
    I_LD Int |
    I_ST Int |
    I_LDR Int |
    I_STR Int |
    I_IMM Int |
    I_SHL Int |
    I_SHR Int |
    I_NOT |
    I_AND |
    I_OR |
    I_GE 
    deriving (Show, Read)

buildInAdd :: [INSHL]
buildInAdd = [LDRC 1, LDRC 1, MRK 0, LDRC 1, LDRC 1, XOR, LDRC 2, LDRC 2, AND, SHL 1, STRC 1, STRC 1, LDRC 0, JZ 1, JMP 0, MRK 1, OR]

addCode :: [INSHL]
addCode = [LDRC 1, LDRC 1, XOR, LDRC 2, LDRC 2, AND, SHL 1, STRC 1, STRC 1, IMM 0, LDRC 1, GE]

inlineAdd' :: Int -> [INSHL] -> [INSHL] -> [INSHL]
inlineAdd' _ [] ac = ac
inlineAdd' i (ADD:xs) ac = inlineAdd' (i + 1) xs $ ac ++ (MRK i : addCode ++ [JZ i, OR])
inlineAdd' i (x:xs) ac = inlineAdd' i xs $ ac ++ [x]

getTar :: INSHL -> Maybe Int
getTar (JMP i) = Just i
getTar (JZ i) = Just i
getTar _ = Nothing

getMark :: INSHL -> Maybe Int
getMark (MRK i) = Just i
getMark _ = Nothing

checkMark :: [INSHL] -> Bool
checkMark func = all (`elem` mapMaybe getMark func) $ mapMaybe getTar func

nextMark :: [INSHL] -> Int
nextMark = maximum . (:) 0 . map (+ 1) . mapMaybe getMark

inlineAdd :: [INSHL] -> [INSHL]
inlineAdd func = if not $ checkMark func then error "unresolved jumps" else inlineAdd' (nextMark func) func []

toIR :: Int -> INSHL -> [INS]
toIR _ (JZ i) = [(I_JZ i)]
toIR _ (JMP i) = [(I_JMP i)]
toIR _ (CAL i) = [(I_CAL i)]
toIR _ (MRK i) = [(I_MRK i)]
toIR _ (LD i) = [(I_LD i)]
toIR _ (LDC i) = [(I_IMM 0), (I_LD i)]
toIR _ (ST i) = [I_ST i]
toIR _ (STC i) = [(I_IMM 0), (I_ST i)]
toIR _ (LDR i) = [(I_LDR i)]
toIR _ (LDRC i) = [(I_IMM 0), (I_LDR i)]
toIR _ (STR i) = [(I_STR i)]
toIR _ (STRC i) = [(I_IMM 0), (I_STR i)]
toIR _ (IMM i) = [(I_IMM i)]
toIR _ (SHL i) = [(I_SHL i)]
toIR _ (SHR i) = [(I_SHR i)]
toIR a (POP) = concatMap (toIR a) [IMM 0, AND, OR]
toIR a (ADD) = I_CAL a : concatMap (toIR a) [STRC 1, POP]
toIR a (SUB) = concatMap (toIR a) [NOT, IMM 1, ADD, ADD]
toIR _ (NOT) = [I_NOT]
toIR _ AND = [I_AND]
toIR _ OR = [I_OR]
toIR x XOR = concatMap (toIR x) [LDRC 1, LDRC 1, OR, LDRC 2, LDRC 2, AND, NOT, AND, STRC 1, POP]
toIR _ GE = [I_GE]
toIR x IMP.EQ = concatMap (toIR x) [LDRC 1, LDRC 1, GE, LDRC 1, LDRC 3, GE, AND, STRC 1, POP]

toBits :: Int -> Int -> [Bool]
toBits len n = map (testBit n) [len - 1,len - 2..0]

encode :: INS -> [Bool]
encode (I_JZ i) = [False, False, False] ++ toBits 8 i
encode (I_JMP i) = [False, False, True] ++ toBits 8 i
encode (I_CAL i) = [False, True, False] ++ toBits 8 i
encode (I_MRK i) = [False, True, True] ++ toBits 8 i

encode (I_LD i) = [True, False, False, False] ++ toBits 7 i
encode (I_ST i) = [True, False, False, True] ++ toBits 7 i
encode (I_LDR i) = [True, False, True, False] ++ toBits 7 i
encode (I_STR i) = [True, False, True, True] ++ toBits 7 i

encode (I_IMM i) = [True, True, False] ++ toBits 8 i

encode (I_SHL i) = [True, True, True, False, False] ++ toBits 6 i
encode (I_SHR i) = [True, True, True, False, True] ++ toBits 6 i

encode (I_NOT) = [True, True, True, True, False, False]
encode (I_AND) = [True, True, True, True, False, True]
encode (I_OR)  = [True, True, True, True, True, False]
encode (I_GE)  = [True, True, True, True, True, True]

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
