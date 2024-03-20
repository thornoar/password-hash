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

shiftAmplifier :: Integer -> Integer
shiftAmplifier = (^5)

charShift :: Char -> Integer
charShift c = shiftAmplifier $ toInteger $ ord c

dropElementInfo :: ([a], Integer) -> (Integer, Integer)
dropElementInfo (src, m) = (toInteger $ length src, m)

addLength :: [a] -> ([a], Integer)
addLength lst = (lst, toInteger $ length lst)

-- A typeclass that defines how elements act on integers for shifting the key in recursive calls
class Shifting a where
    shift :: a -> Integer

-- Characters shift keys by their ACSII values, amplified
instance Shifting Char where
    shift :: Char -> Integer
    shift c = shiftAmplifier $ toInteger $ ord c

-- PRE-DEFINED STRINGS FROM WHICH HASHES WILL BE DRAWN

sourceUpper :: [Char]
sourceUpper = "RQLIANBKJYVWPTEMCZSFDOGUHX"

sourceLower :: [Char]
sourceLower = "ckapzfitqdxnwehrolmbyvsujg"

sourceSpecial :: [Char]
sourceSpecial = "!?%&|#-$+@=*"

sourceNumbers :: [Char]
sourceNumbers = "1952074386"

defaultSources :: [[Char]]
defaultSources = [sourceUpper, sourceLower, sourceSpecial, sourceNumbers]

defaultAmounts :: [Integer]
defaultAmounts = [6, 6, 4, 4]

defaultConfiguration :: [([Char], Integer)]
defaultConfiguration = zip defaultSources defaultAmounts

-- HASH GENERATING FUNCTIONS

-- Choose an ordered sequence of `m` elements from the list `src`.
chooseOrdered :: (Eq a, Shifting a) => Integer -> ([a], Integer) -> [a]
chooseOrdered _ (_, 0) = []
chooseOrdered _ ([], _) = []
chooseOrdered key (src, m)  = curElt : chooseOrdered nextKey (filter (\e -> e /= curElt) src, m-1)
    where
    srcLength = toInteger $ length src
    (keyDiv, keyMod) = divMod key srcLength
    curElt = src !! fromIntegral keyMod
    nextKey = keyDiv + keyMod + shift curElt

-- On the integer segment from 0 to [this] the previous function is injective (in fact bijective)
chooseInjectivityRange :: (Integer, Integer) -> Integer
chooseInjectivityRange pair = factorial' (fst pair) (snd pair)

-- Apply the `chooseOrdered` function to a list, modifying the key every time
mapChooseOrdered :: (Eq a, Shifting a) => Integer -> [([a], Integer)] -> [[a]]
mapChooseOrdered _ [] = []
mapChooseOrdered key srcs = curSelection : mapChooseOrdered nextKey (tail srcs)
    where
    curSrc = head srcs
    (keyDiv, keyMod) = divMod key $ chooseInjectivityRange $ dropElementInfo curSrc
    curSelection = chooseOrdered keyMod curSrc
    keyShift = shiftAmplifier $ (sum . map shift) curSelection
    nextKey = keyDiv + keyMod + keyShift

-- On the integer segment from 0 to [this] the previous function is injective (in fact bijective)
mapChooseInjectivityRange :: [(Integer, Integer)] -> Integer
mapChooseInjectivityRange = product . (map chooseInjectivityRange)

-- Mix a list of lists together, keeping the elements of the individual lists in order.
shuffleLists :: (Eq a, Shifting a) => Integer -> [[a]] -> [a]
shuffleLists _ [] = []
shuffleLists key srcs = (:) curElt $ shuffleLists nextKey $
    (take curIndex srcs)
    ++
    (if (1 < length curLst) then [tail curLst] else [])
    ++
    (drop (curIndex + 1) srcs)
    where
    srcLength = toInteger $ length srcs
    (keyDiv, keyMod) = divMod key srcLength
    curIndex = fromIntegral keyMod
    curLst = srcs !! fromIntegral curIndex
    curElt = head curLst
    nextKey = keyDiv + keyMod + shift curElt

-- On the integer segment from 0 to [this] the previous function is injective. The input is `map length [[a]]`
shuffleInjectivityRange :: [Integer] -> Integer
shuffleInjectivityRange srcs = product $ zipWith (^) [1 .. (toInteger $ length srcs)] (sortDesc srcs)

-- Get a hash sequence from key, shift function and source list
getHash :: (Eq a, Shifting a) => Integer -> [([a], Integer)] -> [a]
getHash key srcs = shuffleLists nextKey hashSelections
    where
    (keyDiv, keyMod) = divMod key $ mapChooseInjectivityRange $ map dropElementInfo srcs
    hashSelections = mapChooseOrdered keyMod srcs
    keyShift = shiftAmplifier $ (sum . map (product . map shift)) hashSelections
    nextKey = keyDiv + keyMod + keyShift

-- All keys between 0 and [this] are guaranteed to give different hashes
getHashInjectivityRange :: [(Integer, Integer)] -> Integer
getHashInjectivityRange pairs = (mapChooseInjectivityRange pairs) * (shuffleInjectivityRange $ map snd pairs)

-- Total theoretical number of distinct hash sequences arising from given source list
numberOfHashes :: [(Integer, Integer)] -> Integer
numberOfHashes srcs = (product $ zipWith cnk fsts snds) * (factorial $ sum snds)
    where
    fsts = map fst srcs
    snds = map snd srcs

-- MANAGING THE PUBLIC KEY
-- Public string must be maximum 32 characters in length to insure injectivity

-- Convert a string to a public key by using the base-128 number system.
getPublicKey :: String -> Integer
getPublicKey "" = 0
getPublicKey (c:cs) = (toInteger $ ord c) * (128 ^ (length cs)) + getPublicKey cs

-- Apply public key to shuffle sources
shuffleSources :: (Eq a, Shifting a) => Integer -> [[a]] -> [[a]]
shuffleSources pkey srcs = mapChooseOrdered pkey (map addLength srcs)

-- BONUS FUNCTIONS

getKeyFromString :: String -> Integer
getKeyFromString = product . (map $ toInteger . ord)

shuffleString :: String -> String
shuffleString str = chooseOrdered (getKeyFromString str) (addLength str)

-- USER INTERFACE

-- Prints help information
helpAction :: String -> [(Integer, Integer)] -> IO ()
helpAction cmd srcs = case cmd of
    "#help" -> do
        putStrLn "usage: phash [#COMMAND [CONFIGURATION] | PUBLIC PRIVATE [CONFIGURATION]]"
        putStrLn ""
        putStrLn "commands:"
        putStrLn "  #help                       show this help message and exit"
        putStrLn "  #hashes [CONFIGURATION]     print the theoretical number of hashes with given configuration"
        putStrLn "  #keys [CONFIGURATION]       print the lower range of significant keys in given configuration"
        putStrLn "  #choices [CONFIGURATION]    print the number of character selections with given configuration"
        putStrLn "  #shuffles [CONFIGURATION]   print the number of selection shufflings with given configuration"
        putStrLn $ "                          (with no CONFIGURATION, commands use " ++ (show defaultConfiguration)
    "#hashes" -> (putStrLn . show) $ numberOfHashes srcs
    "#keys" -> (putStrLn . show) $ getHashInjectivityRange srcs
    "#choices" -> (putStrLn . show) $ mapChooseInjectivityRange srcs
    "#shuffles" -> (putStrLn . show) $ shuffleInjectivityRange $ map snd $ srcs
    _ -> putStrLn "error: help command not recognized"

-- Prints the hash (password) given public and private strings and a hash configuration
hashAction :: String -> String -> [([Char], Integer)] -> IO ()
hashAction publicStr privateStr config = putStrLn $ getHash privateKey shuffledConfig
    where
    publicKey :: Integer
    publicKey = getPublicKey publicStr
    privateKey :: Integer
    privateKey = read $ privateStr
    sources = map fst config
    amounts = map snd config
    shuffledConfig = zip (shuffleSources publicKey sources) amounts

-- The main process
main :: IO ()
main = do
    args <- getArgs
    case (length args) of
        0 -> return ()
        1 ->
            let fullSources = map dropElementInfo defaultConfiguration in
            helpAction (args !! 0) fullSources
        2 -> case (args !! 0) of
            '#':cmd -> helpAction (args !! 0) $ read (args !! 1)
            _ -> hashAction (args !! 0) (args !! 1) defaultConfiguration
        3 -> hashAction (args !! 0) (args !! 1) (read $ args !! 2)
