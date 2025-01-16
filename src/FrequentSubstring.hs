module FrequentSubstring
  ( longestSubstringWithRRepeats
  , mostFrequentSubstringN
  , mostFrequentSubstringHashN
  , substringsLengthN
  , countSubsequencesWithLengthAtLeast
  , countSubsequencesOfLength
  ) where

import FreqMap (mostFrequentInList)

import Prelude hiding (drop, length)
import Data.List (lookup)
import Data.Text (chunksOf, Text, drop, length, singleton, index)
import Data.Hashable (hash)
import Data.Maybe (fromMaybe)

type Hash = Int
type Count = Int

-- ===========================
-- | Find Longest by Repeats
-- ~~~~~~~~~~~~

-- | Precondition: r must be in the interval [1, length t]
longestSubstringWithRRepeats :: Int -> Text -> (Text, Count)
longestSubstringWithRRepeats r t
  | r < 1 = error "longestSubstringWithRRepeats; r cannot be less than 1"
  | r > lt = error "longestSubstringWithRRepeats; r cannot exceed (length t)"
  | otherwise = longestSubstringWithRRepeatsInternal r t upperBound lowerBound
  where lt = length t
        upperBound = (lt, t, 1 ) -- this is meant to be the same as mostFrequentSubstringN lt t
        lowerBound = (1 , singleton (index t 0), lt) -- this is meant to be the same as mostFrequentSubstringN 1 t

type Length = Int
type Repeats = Int
type Bound = (Length, Text, Repeats)
-- | Identifies the longest substring with at least R repeats.
-- | UpperBound and LowerBound correspond to existing subsequences of t, and
-- | their lengths bound the answer's length. That is:
-- | Precondition: upper-bound-length >= answer's length, and
-- |               lower-bound-length <= answer's length.
longestSubstringWithRRepeatsInternal :: Int -> Text -> Bound -> Bound -> (Text, Count)
longestSubstringWithRRepeatsInternal r t u@(ul,ut,ur) l@(ll,lt,lr)
  | ur==r = (ut,ur)
  | gl==ll = (gt,gr)
  | gr >= r = longestSubstringWithRRepeatsInternal r t u g
  | gr <  r = longestSubstringWithRRepeatsInternal r t g l
  where gl = (ul+ll) `div` 2
        (gt,gr) = mostFrequentSubstringN gl t
        g = (gl,gt,gr)

-- ===========================
-- | Find by Length
-- ~~~~~~~~~~~~

-- | Most frequent substring of length n
mostFrequentSubstringN :: Int -> Text -> (Text, Count)
mostFrequentSubstringN n t = (s, count)
  where substrings :: [Text]
        substrings = substringsLengthN n t
        hashSubstringAssList :: [(Int, Text)]
        hashSubstringAssList = ((,) =<< hash) <$> substrings
        (h, count) = mostFrequentInList $ fst <$> hashSubstringAssList
        s = fromMaybe err $ lookup h hashSubstringAssList
        err = error $ "mostFrequentSubstringLengthN; no substrings of length " ++ show n

mostFrequentSubstringHashN :: Int -> Text -> (Hash, Count)
mostFrequentSubstringHashN n t = mostFrequentInList hashes
  where hashes = hash <$> substringsLengthN n t

substringsLengthN :: Int -> Text -> [Text]
substringsLengthN n t = chunksLengthN n `concatMap` suffixes
  where suffixes = (flip drop) t <$> [0..n-1]

chunksLengthN :: Int -> Text -> [Text]
chunksLengthN n t = filter ((n<=) . length) $ chunksOf n t

-- ===========================
-- | Count
-- ~~~~~~~~~~~~

countSubsequencesWithLengthAtLeast :: Int -> Int -> Int
countSubsequencesWithLengthAtLeast sequenceLength threshold
  | sequenceLength < 0 = 0
  | threshold < 0 = countSubsequencesWithLengthAtLeast sequenceLength 0
  | otherwise = ((st+1) * st) `div` 2
  where st = (sequenceLength - threshold + 1)

countSubsequencesOfLength :: Int -> Int -> Int
countSubsequencesOfLength sequenceLength n
  | sequenceLength < 0 = 0
  | n < 0 = 0
  | n > sequenceLength = 0
  | n==0 = 1
  | otherwise = sequenceLength - n + 1

-- ===========================


