module FreqMap
  ( mostFrequentInList
  ) where

import Data.List (foldl')
import Data.IntMap (IntMap, insertWith, foldlWithKey')
import qualified Data.IntMap as IM (empty)

newtype FreqMap = FreqMap (IntMap Count)
  deriving (Show)
type Key = Int
type Count = Int

-- ===========================
-- | Queries
-- ~~~~~~~~~~~~

mostFrequentInList :: [Key] -> (Key, Count)
mostFrequentInList = mostFrequent . fromList

mostFrequent :: FreqMap -> (Key, Count)
mostFrequent (FreqMap m) = foldlWithKey' accum (0,0) m
  where accum :: (Key, Count) -> Key -> Count -> (Key, Count)
        accum acc@(_, ac) k c
          | c > ac = (k,c)
          | otherwise = acc

-- ===========================
-- | Construct
-- ~~~~~~~~~~~~

fromList :: [Key] -> FreqMap
fromList l = foldl' insert empty l

insert :: FreqMap -> Key -> FreqMap
insert (FreqMap m) n = FreqMap $ insertWith (+) n 1 m

empty :: FreqMap
empty = FreqMap IM.empty

-- ===========================

