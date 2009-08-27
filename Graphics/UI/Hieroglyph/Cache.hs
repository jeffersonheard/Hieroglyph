-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Hieroglyph.Cache
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  J.R. Heard
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.UI.Hieroglyph.Cache where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

data Cache k a = Cache { store :: Map.Map k a, times :: IntMap.IntMap k, now :: Int, maxsize :: Int, size :: Int, decimation :: Int }

empty mxsz dec = Cache Map.empty IntMap.empty 0 mxsz 0 dec

get :: Ord k => k -> Cache k a -> (Cache k a,Maybe a)
get key cache = (cache',value)
    where value = Map.lookup key (store cache)
          cache' = maybe (cache{ now = now cache + 1 }) (\_ -> cache{ now = now cache + 1, times = IntMap.insert (now cache) key (times cache) }) value

put :: Ord k => k -> a -> Cache k a -> Cache k a
put key value cache
    | size cache < maxsize cache && (not $ Map.member key (store cache)) = cache { now = now cache + 1, times = IntMap.insert (now cache) key (times cache), store = Map.insert key value (store cache) , size = size cache + 1 }
    | size cache < maxsize cache = cache { now = now cache + 1, times = IntMap.insert (now cache) key (times cache), store = Map.insert key value (store cache) }
    | size cache >= maxsize cache && (Map.member key (store cache)) = cache { now = now cache + 1, times = IntMap.insert (now cache) key (times cache), store = Map.insert key value (store cache) }
    | size cache >= maxsize cache = cache{ now = now cache + 1, times = IntMap.insert (now cache) key times', store = Map.insert key value store', size = size cache - decimation cache }
    where times' = foldr IntMap.delete (times cache) lowtimes
          (lowtimes,lowtimekeys) = unzip . take (decimation cache) . IntMap.toAscList . times $ cache
          store' = foldr Map.delete (store cache) lowtimekeys

put' :: Ord k => k -> a -> Cache k a -> ([a], Cache k a)
put' key value cache
    | size cache < maxsize cache && (not $ Map.member key (store cache)) = ([],cache { now = now cache + 1, times = IntMap.insert (now cache) key (times cache), store = Map.insert key value (store cache) , size = size cache + 1 })
    | size cache < maxsize cache = ([],cache { now = now cache + 1, times = IntMap.insert (now cache) key (times cache), store = Map.insert key value (store cache) })
    | size cache >= maxsize cache && (Map.member key (store cache)) = ([],cache { now = now cache + 1, times = IntMap.insert (now cache) key (times cache), store = Map.insert key value (store cache) })
    | size cache >= maxsize cache = (freed, cache{ now = now cache + 1, times = IntMap.insert (now cache) key times', store = Map.insert key value store', size = size cache - decimation cache })
    where times' = foldr IntMap.delete (times cache) lowtimes
          (lowtimes,lowtimekeys) = unzip . take (decimation cache) . IntMap.toAscList . times $ cache
          store' = foldr Map.delete (store cache) lowtimekeys
          freed = map (store cache Map.!) lowtimekeys

free :: Ord k => Cache k a -> ((k,a),Cache k a)
free cache = ((minkey,minval),cache')
    where minkey = IntMap.findMin (times cache)
          minval = store cache Map.! minkey
          cache' = cache{ now = now cache + 1, times = IntMap.deleteMin (times cache), store = Map.delete minkey (store cache), size = (size cache) }

member :: Ord k => k -> Cache k a -> Bool
member key cache = Map.member key (store cache)

