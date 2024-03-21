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

dropElementInfo :: ([a], Integer) -> (Integer, Integer)
dropElementInfo (src, m) = (toInteger $ length src, m)

nsInYear :: Double
nsInYear = 3.15576E16

-- A typeclass that defines how elements act on integers for shifting the key in recursive calls
class Shifting a where
    shift :: a -> Integer

-- Extending the Shifting typeclass on lists
instance (Shifting a) => Shifting [a] where
    shift = sum . (map shift)

-- Characters shift keys by their ACSII values, amplified
instance Shifting Char where
    shift :: Char -> Integer
    shift c = toInteger $ ord c

mapHashing :: (Shifting c) => Integer -> (Integer -> b -> c) -> (b -> Integer) -> [b] -> [c]
mapHashing _ _ _ [] = []
mapHashing key f spr (b:bs) = res : mapHashing nextKey f spr bs
    where
    (keyDiv, keyMod) = divMod key $ spr b
    res = f keyMod b
    -- keyShift = (sum . map shift) res
    nextKey = keyDiv + shift res

composeHashing :: (Shifting c) => (Integer -> b -> c) -> (b -> Integer) -> (Integer -> c -> d) -> (Integer -> b -> d)
composeHashing f spr g key b = g nextKey c
    where
    (keyDiv, keyMod) = divMod key $ spr b
    c = f keyMod b
    nextKey = keyDiv + shift c

---------------------------------------------------------
-- | PRE-DEFINED STRINGS FROM WHICH HASHES WILL BE DRAWN |
---------------------------------------------------------

sourceUpper :: [Char]
sourceUpper = "RQLIANBKJYVWPTEMCZSFDOGUHX"

sourceLower :: [Char]
sourceLower = "ckapzfitqdxnwehrolmbyvsujg"

sourceSpecial :: [Char]
sourceSpecial = "!@#$%^&*()_-+={[}]|:;<,>?"

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
mapChooseOrdered key = mapHashing key chooseOrdered (chooseInjectivityRange . dropElementInfo)

-- On the integer segment from 0 to [this] the previous function is injective (in fact bijective)
mapChooseInjectivityRange :: [(Integer, Integer)] -> Integer
mapChooseInjectivityRange = product . (map chooseInjectivityRange)

-- Mix a list of lists together, keeping the elements of the individual lists in the same order.
mergeLists :: (Eq a, Shifting a) => Integer -> [[a]] -> [a]
mergeLists _ [] = []
mergeLists key srcs = (:) curElt $ mergeLists nextKey $
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
mergeInjectivityRange :: [Integer] -> Integer
mergeInjectivityRange srcs = product $ zipWith (^) [1 .. (toInteger $ length srcs)] (sortBy (flip compare) srcs)

-- Get a hash sequence from a key and a source configuration
getHash :: (Eq a, Shifting a) => Integer -> [([a], Integer)] -> [a]
getHash = composeHashing mapChooseOrdered (mapChooseInjectivityRange . map dropElementInfo) mergeLists

-- All keys between 0 and [this] are guaranteed to give different hashes
getHashInjectivityRange :: [(Integer, Integer)] -> Integer
getHashInjectivityRange amts = (mapChooseInjectivityRange amts) * (mergeInjectivityRange $ map snd amts)

-----------------------------
-- | MANAGING THE PUBLIC KEY |
-----------------------------

-- Convert a string to a public key by using the base-128 number system.
getPublicKey :: String -> Integer
getPublicKey "" = 0
getPublicKey (c:cs) = (toInteger $ ord c) * (128 ^ (length cs)) + getPublicKey cs

-- Apply public key to merge sources
mergeSources :: (Eq a, Shifting a) => Integer -> [[a]] -> [[a]]
mergeSources pkey srcs = mapChooseOrdered pkey [(src, toInteger $ length src) | src <- srcs]

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
numberOfRepetitions = mergeInjectivityRange

-- Number of private keys that are guaranteed to produce distinct hashes
numberOfPrivateKeys :: [(Integer, Integer)] -> Integer
numberOfPrivateKeys = mapChooseInjectivityRange

-- Number of public keys that are guaranteed to produce distinct hashes
numberOfPublicKeys :: [Integer] -> Integer
numberOfPublicKeys lens = mapChooseInjectivityRange $ zip lens lens

get128PowerLength :: Integer -> Integer
get128PowerLength p = 2*p + 3 * pDiv + npMod
    where
    (pDiv, pMod) = divMod p 28
    npMod
        | pMod < 10 = 1
        | pMod < 19 = 2
        | otherwise = 3

getBiggestPower :: Integer -> Integer -> Integer
getBiggestPower guess bound
    | (get128PowerLength $ guess + 1) < bound = getBiggestPower (guess + 1) bound
    | otherwise = guess + 1

maxLengthOfPublicKey :: [Integer] -> Integer
maxLengthOfPublicKey lens = getBiggestPower 0 $ (toInteger . length . show) (numberOfPublicKeys lens)

--------------------
-- | USER INTERFACE |
--------------------

-- Prints help information
helpAction :: String -> [(Integer, Integer)] -> IO ()
helpAction cmd amts
    | cmd == "--help" || cmd == "-h" = do
        putStrLn "usage: phash [--OPTION [CONFIGURATION] | PUBLIC PRIVATE [CONFIGURATION]]"
        putStrLn ""
        putStrLn "options:"
        putStrLn "  -h, --help                      show this help message and exit"
        putStrLn "  -a, --hashes [CONFIGURATION]    print the theoretical number of hashes"
        putStrLn "  -k, --keys [CONFIGURATION]      print the guaranteed number of keys with distinct hashes"
        putStrLn "  -t, --times [CONFIGURATION]     calculate the number of years required to crack your passwords"
        putStrLn ""
        putStrLn "arguments:"
        putStrLn "  [CONFIGURATION] stands for a list of string-number pairs that defines the synbols used in hashes"
        putStrLn "  PUBLIC stands for public key, a memorable string indicative of the password destination"
        putStrLn "  PRIVATE stands for private key, a large integer known only to the user"
        putStrLn ""
        putStrLn "default configuration:"
        putStrLn $ "  " ++ (show defaultConfiguration)
    | cmd == "--hashes" || cmd == "-a" = do
        putStrLn $ "total theoretical number of hashes:         " ++ (show $ numberOfHashes amts)
        putStrLn $ "range of guaranteed hash injectivity:       " ++ (show $ getHashInjectivityRange amts)
    | cmd == "--keys" || cmd == "-k" = do
        putStrLn $ "number of relevant private keys:           >" ++ (show $ getHashInjectivityRange amts)
        putStrLn $ "number of keys with different hashes:       " ++ (show $ numberOfPrivateKeys amts)
        putStrLn $ "number of keys with the same hash:         >" ++ (show $ numberOfRepetitions $ map snd amts)
        putStrLn $ "maximum relevant length of public keys:     " ++ (show $ maxLengthOfPublicKey $ map fst amts)
    | cmd == "--times" || cmd == "-t" = do
        putStrLn $ "assumed time to check one private key:      " ++ "1 nanosecond = 10^(-9) s"
        putStrLn $ "time required to brute-force your password: " ++
            (show $ floor $ (fromIntegral $ numberOfHashes amts) / nsInYear) ++ " years"
        putStrLn $ "time required to brute-force your key:      " ++
            (show $ floor $ (fromIntegral $ numberOfPrivateKeys amts) / nsInYear) ++ " years"
        putStrLn $ "given the final hash, it is impossible to deduce the private key without brute-forcing."
    | True = putStrLn "error: help command not recognized"

-- Prints the hash (password) given public and private strings and a hash configuration
hashAction :: String -> String -> [([Char], Integer)] -> IO ()
hashAction publicStr privateStr config = putStrLn $ getHash privateKey2 mergedConfig
    where
    publicKey :: Integer
    publicKey = getPublicKey publicStr
    privateBase :: Integer
    privateBase = read $ privateStr
    sources = map fst config
    amounts = map snd config
    (privateKey1, privateKey2) = divMod privateBase $ numberOfRepetitions amounts
    mergedConfig = zip (mergeSources publicKey $ mergeSources privateKey1 sources) amounts

-- The main process
main :: IO ()
main = do
    args <- getArgs
    case (length args) of
        0 -> helpAction "--help" []
        1 -> helpAction (args !! 0) (map dropElementInfo defaultConfiguration)
        2 -> case (args !! 0) of
            '-':cmd -> helpAction (args !! 0) $ read (args !! 1)
            _ -> hashAction (args !! 0) (args !! 1) defaultConfiguration
        3 -> hashAction (args !! 0) (args !! 1) (read $ args !! 2)
        _ -> putStrLn "error: too many arguments"
