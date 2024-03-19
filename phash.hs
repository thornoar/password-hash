import Data.Char (ord)
import Data.List (sortBy)
import System.Environment
import Control.Monad

-- General-purpose functions

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

-- Pre-defined strings from which hashes will be drawn

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

numOfWays :: [(Integer, Integer)] -> Integer
numOfWays pairs = (product $ zipWith cnk fsts snds) * (factorial $ sum snds)
-- numOfWays pairs = (product $ zipWith factorial' fsts snds)
                -- * div (factorial (sum snds)) (product (map factorial snds))
                -- * (product $ zipWith cnk (collect snds) snds)
    where
        fsts = map fst pairs
        snds = map snd pairs

shuffleInjectivityRange :: [[a]] -> Integer
shuffleInjectivityRange lsts = product $ zipWith (^) [1..(toInteger $ length lsts)] (sortDesc $ map length lsts)

-- parseKey :: Integer -> Integer -> (a -> Integer) -> (Integer, Integer)
-- parseKey key n shift = (keyMod, keyDiv + keyMod + shift keyMod)
--     where
--     (keyDiv, keyMod) = divMod key n

charShift :: Char -> Integer
charShift c = (^5) $ toInteger $ ord c

-- Shuffling functions --

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

-- Mix a list of lists together, keeping the elements of the individual lists in order.
shuffleLists :: Integer -> (a -> Integer) -> [[a]] -> [a]
shuffleLists _ _ [] = []
shuffleLists key shift lsts = curElt :
        (shuffleLists nextKey shift $ (take curIndex lsts)
        ++
        (if (1 < length curLst) then [tail curLst] else [])
        ++
        (drop (curIndex + 1) lsts))
    where
    srcLength = toInteger $ length lsts
    (keyDiv, keyMod) = divMod key srcLength
    curIndex = fromIntegral keyMod
    curLst = lsts !! fromIntegral curIndex
    curElt = head curLst
    nextKey = keyDiv + keyMod + shift curElt

-- Convert a string to a public key by using base-128
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
