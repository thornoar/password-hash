import Data.Char (ord)
import Data.List (sortBy)
import System.Environment
import Control.Monad

-- GENERAL-PURPOSE FUNCTIONS

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial (n-1))

factorial' :: Integer -> Integer -> Integer
factorial' n 1 = n
factorial' n m = (n - (m - 1)) * factorial' n (m - 1)

cnk :: Integer -> Integer -> Integer
cnk n k = div (factorial' n k) (factorial k)

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (flip compare)

-- PRE-DEFINED STRINGS FROM WHICH HASHES WILL BE DRAWN

sourceUpper :: [Char]
sourceUpper = "RQLIANBKJYVWPTEMCZSFDOGUHX"

sourceLower :: [Char]
sourceLower = "ckapzfitqdxnwehrolmbyvsujg"

sourceSpecial :: [Char]
sourceSpecial = "!%&#_$+=?*"

sourceNumbers :: [Char]
sourceNumbers = "1952074386"

defaultPasswordLength :: Int
defaultPasswordLength = 20

-- parseKey :: Integer -> Integer -> (a -> Integer) -> (Integer, Integer)
-- parseKey key n shift = (keyMod, keyDiv + keyMod + shift keyMod)
--     where (keyDiv, keyMod) = divMod key n

shiftAmplifier :: Integer -> Integer
shiftAmplifier = (^5)

charShift :: Char -> Integer
charShift c = shiftAmplifier $ toInteger $ ord c

-- HASH GENERATING FUNCTIONS

-- Choose an ordered sequence of `m` elements from the list `src`.
chooseOrdered :: (Eq a) => Integer -> (a -> Integer) -> ([a], Integer) -> [a]
chooseOrdered _ _ (_, 0) = []
chooseOrdered _ _ ([], _) = []
chooseOrdered key shift (src, m)  = curElt : chooseOrdered nextKey shift (filter (\e -> e /= curElt) src, m-1)
    where
    srcLength = toInteger $ length src
    (keyDiv, keyMod) = divMod key srcLength
    curElt = src !! fromIntegral keyMod
    nextKey = keyDiv + keyMod + shift curElt

-- On the segment from 0 to [this] the previous function is injective (in fact bijective)
chooseInjectivityRange :: ([a], Integer) -> Integer
chooseInjectivityRange (src, m) = factorial' (toInteger $ length src) m

mapChooseOrdered :: (Eq a) => Integer -> (a -> Integer) -> [([a], Integer)] -> [[a]]
mapChooseOrdered _ _ [] = []
mapChooseOrdered key shift srcs = curSelection : mapChooseOrdered nextKey shift (tail srcs)
    where
    curSrc = head srcs
    (keyDiv, keyMod) = divMod key $ chooseInjectivityRange curSrc
    curSelection = chooseOrdered keyMod shift curSrc
    keyShift = shiftAmplifier $ (sum . map shift) curSelection
    nextKey = keyDiv + keyMod + keyShift

-- On the segment from 0 to [this] the previous function is injective (in fact bijective)
mapChooseInjectivityRange :: [([a], Integer)] -> Integer
mapChooseInjectivityRange = product . (map chooseInjectivityRange)

-- Mix a list of lists together, keeping the elements of the individual lists in order.
shuffleLists :: (Eq a) => Integer -> (a -> Integer) -> [[a]] -> [a]
shuffleLists _ _ [] = []
shuffleLists key shift srcs = curElt :
    (shuffleLists nextKey shift $ (take curIndex srcs)
    ++ (if (1 < length curLst) then [tail curLst] else [])
    ++ (drop (curIndex + 1) srcs))
    where
    srcLength = toInteger $ length srcs
    (keyDiv, keyMod) = divMod key srcLength
    curIndex = fromIntegral keyMod
    curLst = srcs !! fromIntegral curIndex
    curElt = head curLst
    nextKey = keyDiv + keyMod + shift curElt

-- On the segment from 0 to [this] the previous function is injective
shuffleInjectivityRange :: [[a]] -> Integer
shuffleInjectivityRange srcs = product $ zipWith (^) [1..(toInteger $ length srcs)] (sortDesc $ map length srcs)

-- Get a hash sequence from key, shift function and source list
getHash :: (Eq a) => Integer -> (a -> Integer) -> [([a], Integer)] -> [a]
getHash key shift srcs = shuffleLists nextKey shift hashSelections
    where
    (keyDiv, keyMod) = divMod key $ mapChooseInjectivityRange srcs
    hashSelections = mapChooseOrdered keyMod shift srcs
    keyShift = shiftAmplifier $ (sum . map (product . map shift)) hashSelections
    nextKey = keyDiv + keyMod + keyShift

-- Total theoretical number of distinct hash sequences arising from given source list
numberOfHashes :: [([a], Integer)] -> Integer
numberOfHashes srcs = (product $ zipWith cnk fsts snds) * (factorial $ sum snds)
    where
    fsts = map (toInteger . length . fst) srcs
    snds = map snd srcs

-- Convert a string to a public key by using the base-128 number system.
getPublicKey :: String -> Integer
getPublicKey "" = 0
getPublicKey (c:cs) = (toInteger $ ord c) * (128 ^ (length cs)) + getPublicKey cs

-- replace :: String -> (Char, Char) -> String
-- replace "" _ = ""
-- replace (c:str) (k,v)
--     | c == k = v : replace str (k,v)
--     | otherwise = c : replace str (k,v)
--
-- replace' :: String -> [(Char, Char)] -> String
-- replace' str [] = str
-- replace' str ((k,v):kvs) = replace' (replace str (k,v)) kvs
--
-- includeInString :: Integer -> String -> String -> String
-- includeInString key str incl = 
--     let
--         replaceCount = toInteger $ min (div (length str) 4) (length incl)
--         strChars = chooseOrdered key str replaceCount
--         inclChars = chooseOrdered key incl replaceCount
--         charMap = zip strChars inclChars
--     in
--         replace' str charMap
--
-- includeInString' :: Integer -> String -> [String] -> String
-- includeInString' _ str [] = str
-- includeInString' key str (incl:incls) = includeInString' key (includeInString key str incl) incls

-- getPassword :: Integer -> Integer -> String
-- getPassword key len = includeInString' key
--     (chooseOrdered key sourceString len)
--     [sourceNumbers, sourceSpecial]

-- main :: IO ()
-- main = do
--     args <- getArgs
--     -- sequence_ $ map putStrLn args
--     putStrLn $ getPassword (read (args !! 0)) 20

main = putStrLn "it compiled!"
