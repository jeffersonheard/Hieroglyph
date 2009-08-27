{-# LANGUAGE NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
--
-- Module      :  Data.Caching.Memory.LRU
-- Copyright   :  Renaissance Computing Institute
-- License     :  BSD3
--
-- Maintainer  :  J.R. Heard
-- Stability   :  Beta
-- Portability :  GHC 
--
-- | A simple in memory LRU cache.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Hieroglyph.Cache where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

data Cache k a = Cache 
    { store :: Map.Map k a
    , times :: IntMap.IntMap k
    , now :: Int
    , maxsize :: Int
    , size :: Int
    , decimation :: Int } deriving Show

-- | @get key cache@ Gets a value out of the cache.  Returns both the value and 
--   an updated cache reflecting that the times have been updated.
get :: Ord k => k -> Cache k a -> (Cache k a,Maybe a)
get key cache = (cache',value)
    where value = Map.lookup key (store cache)
          cache' = maybe (cache{ now = now cache + 1 }) 
                         (\_ -> cache{ now = now cache + 1
                                     , times = IntMap.insert (now cache) key . IntMap.filter (/=key) . times $ cache }) 
                         value

-- | @put cache list@. Put a list of key-value pairs in the cache.  Returns the udpated cache.
putList :: Ord k => Cache k a -> [(k,a)] -> Cache k a
putList = foldr (\(a,b) m -> put a b m)

-- | @put key value cache@. Put a value in the cache by key.  If the key already exists, the
--   value for that key will be replaced.  If the cache is full, some values will drop off the
--   end of it silently.
put :: Ord k => k -> a -> Cache k a -> Cache k a
put key value cache
    | size cache < maxsize cache && (not $ Map.member key (store cache)) = cache 
        { now = now cache + 1
        , times = IntMap.insert (now cache) key . times $ cache
        , store = Map.insert key value (store cache) 
        , size = size cache + 1 }
    | size cache < maxsize cache = cache 
        { now = now cache + 1
        , times = IntMap.insert (now cache) key . IntMap.filter (/=key) . times $ cache
        , store = Map.insert key value (store cache) }
    | size cache >= maxsize cache && (Map.member key (store cache)) = cache 
        { now = now cache + 1
        , times = IntMap.insert (now cache) key . IntMap.filter (/=key) . times $ cache
        , store = Map.insert key value (store cache) }
    | size cache >= maxsize cache = cache 
        { now = now cache + 1
        , times = IntMap.insert (now cache) key times'
        , store = Map.insert key value store'
        , size = size cache - decimation cache }
    where times' = foldr IntMap.delete (times cache) lowtimes
          (lowtimes,lowtimekeys) = unzip . take (decimation cache) . IntMap.toAscList . times $ cache
          store' = foldr Map.delete (store cache) lowtimekeys

-- | @put' key value cache@. Put a key in the cache.  If the cache is full, then
--   a number of values will be expunged from the cache.  Those values will be 
--   returned as the first value in the pair of (values expunged, updated cache)
put' :: Ord k => k -> a -> Cache k a -> ([a], Cache k a)
put' key value cache
    | size cache < maxsize cache && (not $ Map.member key (store cache)) = ([]   ,cache 
        { now = now cache + 1
        , times = IntMap.insert (now cache) key . times $ cache
        , store = Map.insert key value (store cache) 
        , size = size cache + 1 })
    | size cache < maxsize cache = ([]   ,cache 
        { now = now cache + 1
        , times = IntMap.insert (now cache) key . IntMap.filter (/=key) . times $ cache
        , store = Map.insert key value (store cache) })
    | size cache >= maxsize cache && (Map.member key (store cache)) = ([]   ,cache 
        { now = now cache + 1
        , times = IntMap.insert (now cache) key . IntMap.filter (/=key) . times $ cache
        , store = Map.insert key value (store cache) })
    | size cache >= maxsize cache = (freed, cache 
        { now = now cache + 1
        , times = IntMap.insert (now cache) key times'
        , store = Map.insert key value store'
        , size = size cache - decimation cache })
    where times' = foldr IntMap.delete (times cache) lowtimes
          (lowtimes,lowtimekeys) = unzip . take (decimation cache) . IntMap.toAscList . times $ cache
          store' = foldr Map.delete (store cache) lowtimekeys
          freed = map (store cache Map.!) lowtimekeys
 
-- | @free cache@. Frees a single value from the cache.  If the cache is empty,
--   free will error out, so be sure ot check isEmpty first. 
free :: Ord k => Cache k a -> ((k,a),Cache k a)
free cache = 
    ( (minkey,minval)
    , cache' )
    where minkey = IntMap.findMin (times cache)
          minval = store cache Map.! minkey
          cache' = cache{ now = now cache + 1
                        , times = IntMap.deleteMin (times cache)
                        , store = Map.delete minkey (store cache)
                        , size = size cache - 1 }
                        
freeN :: Ord k => Int -> Cache k a -> ([(k,a)], Cache k a)
freeN n cache = (zip lowtimekeys freed, cache{
          now = now cache + 1
        , times = times'
        , store = store'
        , size = size cache - n })
   where times' = foldr IntMap.delete (times cache) lowtimes
         (lowtimes,lowtimekeys) = unzip . take (decimation cache) . IntMap.toAscList . times $ cache
         store' = foldr Map.delete (store cache) lowtimekeys
         freed = map (store cache Map.!) lowtimekeys 


-- | @empty maxsize decimationk@ Creates a new empty cache with maximum size /maxsize/
--   and the number of keys to remove upon /putting/ to a full cache equal to /decimationk/.
empty :: Int -> Int -> Cache k a
empty mxsz dec = Cache Map.empty IntMap.empty 0 mxsz 0 dec

-- | membership test for a key
member :: Ord k => k -> Cache k a -> Bool
member key cache = Map.member key (store cache)

-- | get all keys in the cache
keys :: Ord k => Cache k a -> [k]
keys = Map.keys . store

-- | get all values in the cache
elems :: Ord k => Cache k a -> [a]
elems = Map.elems . store

-- | check to see if the cache is empty
null :: Ord k => Cache k a -> Bool
null x = Map.null . store $ x

