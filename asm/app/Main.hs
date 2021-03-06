module Main where

import Control.Arrow
import System.Environment
import System.Endian
import Debug.Trace
import Data.Word
import Data.Bits
import Data.Maybe
import qualified Data.List as L
import qualified Data.ByteString as BS
import System.Console.GetOpt

import IMP

buildHeader :: [Int] -> [Word8]
buildHeader flen = trace (show flen) . concatMap octets64 $ numFuns : reverse starts
    where numFuns = length flen
          starts = L.foldl' (\ spos len -> (8 + len + head spos) : spos) [(numFuns + 2) * 8] flen

octets64 :: Int -> [Word8]
octets64 w = 
    [ fromIntegral (w `shiftR` i) | i <- [56,48..0]]

buildFun :: Int -> [Word8] -> [Word8]
buildFun len dat = octets64 len ++ dat

buildData :: Int -> [Int] -> [Word8]
buildData mle fdat = concatMap octets64 $ (length fdat) : (max mle $ length fdat) : fdat

assemble :: Prog -> [Word8]
assemble pr = trace (unlines $ map show funsLow) $ buildHeader (map length funsAss)
        ++ concatMap (uncurry buildFun) (zip (map length funsLow) funsAss)
        ++ buildData (datMLen pr) (dat pr)
    where numFuns = length $ funs pr
          funsLow = map (concatMap (toIR numFuns)) $ funs pr ++ if any (not . null . filter (== ADD)) (funs pr) then [buildInAdd] else []
          funsAss = map (toInts . encodeAll) funsLow

splitIntoFuns :: [String] -> [String] -> [[String]] -> [[String]]
splitIntoFuns [] ac acF = reverse . map reverse $ ac : acF
splitIntoFuns (l:ls) ac acF | l == "--fun--" = splitIntoFuns ls [] (ac:acF)
                            | otherwise = splitIntoFuns ls (l:ac) acF

readAsm :: [String] -> Prog
readAsm lns = Prog (map (map read) funTxt) fdatRd $ length fdatRd
    where (fns, fdat) = second (drop 1) $ break (== "--data--") lns
          fdatRd = (map read $ filter (not . null) fdat)
          funTxt = filter (not . null) . map (filter (not . null)) $ splitIntoFuns fns [] []

main :: IO ()
main = do
    (opt, _) <- asmOpts =<< getArgs
    p <- (readAsm . lines) <$> (readFile . fromJust $ optInput opt)
    let p' = if optInlineAdd opt then p {funs = map inlineAdd $ funs p} else p
    BS.writeFile (fromJust $ optOutput opt) . BS.pack $ assemble p'

data Options = Options {
    optOutput      :: Maybe FilePath,
    optInput       :: Maybe FilePath,
    optInlineAdd   :: Bool
    } deriving (Show)

defaultOptions :: Options
defaultOptions = Options Nothing Nothing False

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['o'] ["output"]
        (ReqArg (\ f opts -> opts { optOutput = Just f })
                "FILE")
        "output FILE"
    , Option ['c'] []
        (ReqArg (\ f opts -> opts { optInput = Just f })
                "FILE")
        "input FILE"
    , Option ['i'] ["inline-add"]
        (NoArg (\ opts -> opts { optInlineAdd = True }))
        "attempt to inline additions"
    ]

asmOpts :: [String] -> IO (Options, [String])
asmOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: asm [OPTION...]"