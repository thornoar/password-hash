import Data.Char (ord)
import Data.List (sortBy)
import System.Environment (getArgs)

-------------------------------
-- | GENERAL-PURPOSE FUNCTIONS |
-------------------------------

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial (n-1))

factorial' :: Integer -> Integer -> Integer
factorial' n 1 = n
factorial' n m = (n - (m - 1)) * factorial' n (m - 1)

cnk :: Integer -> Integer -> Integer
cnk n k
    | k < 0 = 0
    | k == 0 = 1
    | k == n = 1
    | k > n = 0
    | otherwise = (cnk (n-1) (k-1)) + (cnk (n-1) k)
-- cnk n k = div (factorial' n k) (factorial k)

shiftAmplifier :: Integer -> Integer
-- shiftAmplifier = (^5)
shiftAmplifier = id

dropElementInfo :: ([a], Integer) -> (Integer, Integer)
dropElementInfo (src, m) = (toInteger $ length src, m)

addLength :: [a] -> ([a], Integer)
addLength lst = (lst, toInteger $ length lst)

nsInYear :: Double
nsInYear = 3.15576E16

-- lowerSqrt :: Integer -> Integer
-- lowerSqrt = (+ (-1)) . floor . sqrt . fromIntegral

-- A typeclass that defines how elements act on integers for shifting the key in recursive calls
class Shifting a where
    shift :: a -> Integer

-- Characters shift keys by their ACSII values, amplified
instance Shifting Char where
    shift :: Char -> Integer
    shift c = shiftAmplifier $ toInteger $ ord c

---------------------------------------------------------
-- | PRE-DEFINED STRINGS FROM WHICH HASHES WILL BE DRAWN |
---------------------------------------------------------

sourceUpper :: [Char]
sourceUpper = "RQLIANBKJYVWPTEMCZSFDOGUHX"

sourceLower :: [Char]
sourceLower = "ckapzfitqdxnwehrolmbyvsujg"

sourceSpecial :: [Char]
-- sourceSpecial = "!?%&|#-$+@=*"
sourceSpecial = "~!@#$%^&*()_-+={[}]|:;'<,>.?/"

sourceNumbers :: [Char]
sourceNumbers = "1952074386"

defaultConfiguration :: [([Char], Integer)]
defaultConfiguration = [(sourceUpper, 6), (sourceLower, 6), (sourceSpecial, 4), (sourceNumbers, 4)]

-------------------------------
-- | HASH GENERATING FUNCTIONS |
-------------------------------

-- Choose an ordered sequence of `m` elements from the list `src`.
chooseOrdered :: (Eq a, Shifting a) => Integer -> ([a], Integer) -> [a]
chooseOrdered _ (_, 0) = []
chooseOrdered _ ([], _) = []
chooseOrdered key (src, m)  = curElt : chooseOrdered nextKey (filter (\e -> e /= curElt) src, m-1)
    where
    srcLength = toInteger $ length src
    (keyDiv, keyMod) = divMod key srcLength
    curElt = src !! fromIntegral keyMod
    nextKey = keyDiv + shift curElt

-- On the integer segment from 0 to [this] the previous function is injective (in fact bijective)
chooseInjectivityRange :: (Integer, Integer) -> Integer
chooseInjectivityRange amt = factorial' (fst amt) (snd amt)

-- Apply the `chooseOrdered` function to a list, modifying the key every time
mapChooseOrdered :: (Eq a, Shifting a) => Integer -> [([a], Integer)] -> [[a]]
mapChooseOrdered _ [] = []
mapChooseOrdered key config = curSelection : mapChooseOrdered nextKey (tail config)
    where
    curSrc = head config
    (keyDiv, keyMod) = divMod key $ chooseInjectivityRange $ dropElementInfo curSrc
    curSelection = chooseOrdered keyMod curSrc
    keyShift = shiftAmplifier $ (sum . map shift) curSelection
    nextKey = keyDiv + keyShift

-- On the integer segment from 0 to [this] the previous function is injective (in fact bijective)
mapChooseInjectivityRange :: [(Integer, Integer)] -> Integer
mapChooseInjectivityRange = product . (map chooseInjectivityRange)

-- Mix a list of lists together, keeping the elements of the individual lists in the same order.
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
    nextKey = keyDiv + shift curElt

-- On the integer segment from 0 to [this] the previous function is injective.
shuffleInjectivityRange :: [Integer] -> Integer
shuffleInjectivityRange srcs = product $ zipWith (^) [1 .. (toInteger $ length srcs)] (sortBy (flip compare) srcs)

-- Get a hash sequence from a key and a source configuration
getHash :: (Eq a, Shifting a) => Integer -> [([a], Integer)] -> [a]
getHash key config = shuffleLists nextKey hashSelections
    where
    (keyDiv, keyMod) = divMod key $ mapChooseInjectivityRange $ map dropElementInfo config
    hashSelections = mapChooseOrdered keyMod config
    keyShift = shiftAmplifier $ (sum . map (product . map shift)) hashSelections
    nextKey = keyDiv + keyShift

-- All keys between 0 and [this] are guaranteed to give different hashes
getHashInjectivityRange :: [(Integer, Integer)] -> Integer
getHashInjectivityRange amts = (mapChooseInjectivityRange amts) * (shuffleInjectivityRange $ map snd amts)

-----------------------------
-- | MANAGING THE PUBLIC KEY |
-----------------------------

-- Convert a string to a public key by using the base-128 number system.
getPublicKey :: String -> Integer
getPublicKey "" = 0
getPublicKey (c:cs) = (toInteger $ ord c) * (128 ^ (length cs)) + getPublicKey cs

-- Apply public key to shuffle sources
shuffleSources :: (Eq a, Shifting a) => Integer -> [[a]] -> [[a]]
shuffleSources pkey srcs = mapChooseOrdered pkey (map addLength srcs)

----------------------
-- | COUNTING NUMBERS |
----------------------

-- Total theoretical number of distinct hash sequences arising from given source list
numberOfHashes :: [(Integer, Integer)] -> Integer
numberOfHashes amts = (product $ zipWith cnk fsts snds) * (factorial $ sum snds)
    where
    fsts = map fst amts
    snds = map snd amts
    
-- Approximately [this] many keys will produce the same hash
numberOfRepetitions :: [Integer] -> Integer
numberOfRepetitions = shuffleInjectivityRange

-- Number of private keys that are guaranteed to produce distinct hashes
numberOfPrivateKeys :: [(Integer, Integer)] -> Integer
numberOfPrivateKeys = mapChooseInjectivityRange

-- Number of public keys that are guaranteed to produce distinct hashes
numberOfPublicKeys :: [Integer] -> Integer
numberOfPublicKeys lens = mapChooseInjectivityRange $ zip lens lens

--------------------
-- | USER INTERFACE |
--------------------

-- Prints help information
helpAction :: String -> [(Integer, Integer)] -> IO ()
helpAction cmd amts = case cmd of
    "#help" -> do
        putStrLn "usage: phash [#COMMAND [CONFIGURATION] | >COMMAND ARGUMENT | PUBLIC PRIVATE [CONFIGURATION]]"
        putStrLn ""
        putStrLn "commands:"
        putStrLn "  #help                       show this help message and exit"
        putStrLn "  #hashes [CONFIGURATION]     print the theoretical number of hashes with given configuration"
        putStrLn "  #keys [CONFIGURATION]       print the lower range of significant keys in given configuration"
        putStrLn "  #choices [CONFIGURATION]    print the number of character selections with given configuration"
        putStrLn "  #shuffles [CONFIGURATION]   print the number of selection shufflings with given configuration"
        putStrLn ""
        putStrLn "default configuration:"
        putStrLn $ "  " ++ (show defaultConfiguration)
    "#hashes" -> do
        putStrLn $ "total theoretical number of hashes:     " ++ (show $ numberOfHashes amts)
        putStrLn $ "range of guaranteed hash injectivity:   " ++ (show $ getHashInjectivityRange amts)
    "#keys" -> do
        putStrLn $ "number of relevant private keys:       >" ++ (show $ getHashInjectivityRange amts)
        putStrLn $ "number of keys with different hashes:   " ++ (show $ numberOfPrivateKeys amts)
        putStrLn $ "number of keys with the same hash:     >" ++ (show $ numberOfRepetitions $ map snd amts)
    "#times" -> do
        putStrLn $ "assumed time to check one private key:  " ++ "1 nanosecond = 10^(-9) s"
        putStrLn $ "time required to brute-force your key:  " ++
            (show $ floor $ (fromIntegral $ numberOfPrivateKeys amts) / nsInYear) ++ " years"
        putStrLn $ "given the final hash, it is impossible to deduce the private key without brute-forcing."
    _ -> putStrLn "error: help command not recognized"

-- Prints the hash (password) given public and private strings and a hash configuration
hashAction :: String -> String -> [([Char], Integer)] -> IO ()
hashAction publicStr privateStr config = putStrLn $ getHash privateKey2 shuffledConfig
    where
    publicKey :: Integer
    publicKey = getPublicKey publicStr
    privateBase :: Integer
    privateBase = read $ privateStr
    sources = map fst config
    amounts = map snd config
    (privateKey1, privateKey2) = divMod privateBase $ numberOfRepetitions amounts
    shuffledConfig = zip (shuffleSources publicKey $ shuffleSources privateKey1 sources) amounts

-- The main process
main :: IO ()
main = do
    args <- getArgs
    case (length args) of
        0 -> helpAction "#help" []
        1 -> helpAction (args !! 0) (map dropElementInfo defaultConfiguration)
        2 -> case (args !! 0) of
            '#':cmd -> helpAction (args !! 0) $ read (args !! 1)
            _ -> hashAction (args !! 0) (args !! 1) defaultConfiguration
        3 -> hashAction (args !! 0) (args !! 1) (read $ args !! 2)
        _ -> putStrLn "error: too many arguments"
