import Data.Char (ord)
import System.Environment (getArgs)

-- ┌───────────────────────────┐
-- │ GENERAL-PURPOSE FUNCTIONS │
-- └───────────────────────────┘

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial (n-1))

factorial' :: Integer -> Integer -> Integer
factorial' n 1 = n
factorial' n m = (n - (m - 1)) * factorial' n (m - 1)

cnk :: Integer -> Integer -> Integer
cnk n k = div (factorial' n k) (factorial k)

len :: [a] -> Integer
len = toInteger . length

dropElementInfo :: ([a], Integer) -> (Integer, Integer)
dropElementInfo (src, m) = (len src, m)

mapHashing :: (Shifting b) => (Integer -> a -> b) -> (a -> Integer) -> (Integer -> [a] -> [b])
mapHashing _ _ _ [] = []
mapHashing f spr key (a:as) = res : mapHashing f spr nextKey as
    where
    (keyDiv, keyMod) = divMod key $ spr a
    res = f keyMod a
    nextKey = keyDiv + shift res

composeHashing :: (Integer -> a -> b) -> (Integer -> b -> c) -> (Integer -> Integer -> a -> c)
composeHashing f g key1 key2 a = g key2 $ f key1 a

-- A typeclass that defines how elements act on integers for shifting the key in recursive calls
class Shifting a where
    shift :: a -> Integer

-- Extending the Shifting typeclass on lists
instance (Shifting a) => Shifting [a] where
    shift = sum . (map shift)

-- Characters shift keys by their ACSII values, amplified
instance Shifting Char where
    shift = toInteger . ord

-- ┌─────────────────────────────────────────────────────┐
-- │ PRE-DEFINED STRINGS FROM WHICH HASHES WILL BE DRAWN │
-- └─────────────────────────────────────────────────────┘

sourceLower :: [Char]
sourceLower = "ckapzfitqdxnwehrolmbyvsujg"

sourceUpper :: [Char]
sourceUpper = "RQLIANBKJYVWPTEMCZSFDOGUHX"

sourceSpecial :: [Char]
sourceSpecial = "=!*@?$%#&-+^"

sourceNumbers :: [Char]
sourceNumbers = "1952074386"

defaultConfiguration :: [([Char], Integer)]
defaultConfiguration = [(sourceLower, 10), (sourceUpper, 10), (sourceSpecial, 5), (sourceNumbers, 5)]

defaultAmounts :: [(Integer, Integer)]
defaultAmounts = map dropElementInfo defaultConfiguration

-- ┌───────────────────────────┐
-- │ HASH GENERATING FUNCTIONS │
-- └───────────────────────────┘

-- Choose an ordered sequence of `m` elements from the list `src`.
chooseOrdered :: (Eq a, Shifting a) => Integer -> ([a], Integer) -> [a]
chooseOrdered _ (_, 0) = []
chooseOrdered _ ([], _) = []
chooseOrdered key (src, m)  = curElt : chooseOrdered nextKey (filter (\e -> e /= curElt) src, m-1)
    where
    (keyDiv, keyMod) = divMod key $ len src
    curElt = src !! fromIntegral keyMod
    nextKey = keyDiv + shift curElt

-- On the integer segment from 0 to [this] the previous function is injective (in fact bijective)
chooseSpread :: (Integer, Integer) -> Integer
chooseSpread (n, m) = factorial' n m

-- Get a hash sequence from a key and a source configuration
getHash :: (Eq a, Shifting a) => Integer -> Integer -> [([a], Integer)] -> [a]
getHash = composeHashing (mapHashing chooseOrdered (chooseSpread . dropElementInfo)) shuffleList
    where
    shuffleList :: (Eq a, Shifting a) => Integer -> [[a]] -> [a]
    shuffleList key srcs = chooseOrdered key (src, len src)
        where src = concat srcs

-- ┌─────────────────────────┐
-- │ MANAGING THE PUBLIC KEY │
-- └─────────────────────────┘

-- Convert a string to a public key by using the base-128 number system.
getPublicKey :: String -> Integer
getPublicKey "" = 0
getPublicKey (c:cs) = (toInteger $ ord c) * (128 ^ (length cs)) + getPublicKey cs

-- ┌──────────────────┐
-- │ COUNTING NUMBERS │
-- └──────────────────┘

-- Total theoretical number of distinct hash sequences arising from given source list
numberOfHashes :: [(Integer, Integer)] -> Integer
numberOfHashes amts = (product $ zipWith cnk fsts snds) * (factorial $ sum snds)
    where
    fsts = map fst amts
    snds = map snd amts

-- Number of private keys that are guaranteed to produce distinct hashes
numberOfPrivateChoiceKeys :: [(Integer, Integer)] -> Integer
numberOfPrivateChoiceKeys = product . map chooseSpread

-- Number of private keys that are guaranteed to produce distinct hashes
numberOfPrivateShuffleKeys :: [Integer] -> Integer
numberOfPrivateShuffleKeys = factorial . sum

-- Approximately [this] many keys will produce the same hash
numberOfRepetitions :: [Integer] -> Integer
numberOfRepetitions = product . map factorial

-- Number of public keys that are guaranteed to produce distinct hashes
numberOfPublicKeys :: [(Integer, Integer)] -> Integer
numberOfPublicKeys = numberOfPrivateChoiceKeys

-- maxLengthOfPublicKey :: [Integer] -> Integer
maxLengthOfPublicKey :: [(Integer, Integer)] -> Integer
maxLengthOfPublicKey amts = getBiggestPower 0 $ (len . show) (numberOfPublicKeys amts)
    where
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

timeToCheck :: Double
timeToCheck = 1.0
psInYear :: Double
psInYear = 3.15576E19
ageOfUniverse :: Double
ageOfUniverse = 13.787E9

timeToCrack :: Integer -> (Integer, Integer)
timeToCrack num = (floor inYears, floor inAgesOfUniverse)
    where
    inYears = (fromIntegral num) * timeToCheck / psInYear
    inAgesOfUniverse = inYears / ageOfUniverse

-- ┌────────────────┐
-- │ USER INTERFACE │
-- └────────────────┘

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
    | cmd == "--keys" || cmd == "-k" = do
        putStrLn $ "number of choice keys:                      " ++ (show $ numberOfPrivateChoiceKeys amts)
        putStrLn $ "number of shuffle keys:                     " ++ (show $ numberOfPrivateShuffleKeys $ map snd amts)
        putStrLn $ "number of key pairs with the same hash:     " ++ (show $ numberOfRepetitions $ map snd amts)
        putStrLn $ "maximum relevant length of the public key:  " ++ (show $ maxLengthOfPublicKey amts)
    | cmd == "--times" || cmd == "-t" = do
        putStrLn $ "assumed time to check one private key:      " ++ "1 picosecond = 10^(-12) s"
        putStrLn $ let (inY, inAoU) = timeToCrack $ numberOfHashes amts
                in "time to brute-force your password:          " ++ (show inY) ++ " years\n" ++
                   "                                         or " ++ (show inAoU) ++ " ages of the Universe"
        putStrLn $ let (inY, inAoU) = timeToCrack $ numberOfPrivateChoiceKeys amts
                in "time to brute-force your first key:         " ++ (show inY) ++ " years\n" ++
                   "                                         or " ++ (show inAoU) ++ " ages of the Universe"
        putStrLn $ let (inY, inAoU) = timeToCrack $ numberOfPrivateShuffleKeys $ map snd amts
                in "time to brute-force your second key:        " ++ (show inY) ++ " years\n" ++
                   "                                         or " ++ (show inAoU) ++ " ages of the Universe"
        putStrLn $ "given the final hash, it is impossible to deduce the private keys without brute-forcing."
    | True = putStrLn "error: help command not recognized"

-- Prints the hash (password) given public and private strings and a hash configuration
hashAction :: String -> String -> String -> [([Char], Integer)] -> IO ()
hashAction publicStr pcs pss config = putStrLn $ getHash privateChoiceKey privateShuffleKey config
    where
    publicKey :: Integer
    publicKey = getPublicKey publicStr
    privateChoiceKey :: Integer
    privateChoiceKey = mod (publicKey + read pcs) $ (numberOfPrivateChoiceKeys . map dropElementInfo) config
    privateShuffleKey :: Integer
    privateShuffleKey = read pss

-- The main process
main :: IO ()
main = do
    args <- getArgs
    case (length args) of
        0 -> helpAction "--help" []
        1 -> helpAction (args !! 0) $ map dropElementInfo defaultConfiguration
        2 -> helpAction (args !! 0) $ read (args !! 1)
        3 -> hashAction (args !! 0) (args !! 1) (args !! 2) defaultConfiguration
        4 -> hashAction (args !! 0) (args !! 1) (args !! 2) (read $ args !! 3)
        _ -> putStrLn "error: too many arguments"
