module Main where

import Data.Maybe
import qualified Data.PSQueue as PSQ
import qualified Data.Map as Map
import qualified Data.Set as Set

-- O((V + E) log V) or O(V log V + E) for Fibonacci heaps
-- dijkstras(s,G)
--      Q = Heap()                                          O(1)
--      visited = set()                                     O(1)
--      weights = map()                                     O(1)
--
--      Q.put(s, 0)
--      while !Q.empty                                      O(V), since we aren't doing decrease key
--              n,w = Q.pop_min() # return this             O(V * log V)
--              weights[n] = w                              O(V * 1)
--              visited.add(n)                              O(V * 1)
--              for (w_m, m) in neighbors of n in G         O(E)
--                      if m visited: continue              O(E * 1)
--                      Q.insert_or_decrease(m, w + w_m)    O(E * log V) -- can be O(E) with Fibonacci heap

type Node = String
-- adjacency list
type Graph = Map.Map Node [(Float, Node)]

-- This function computes weights of the "remaining" nodes, where remaining is not yet visited
dijkstras :: Graph -> Node -> Map.Map Node Float
dijkstras graph start = dijkstras_helper Set.empty (PSQ.singleton start 0) Map.empty
  where
    dijkstras_helper visited frontier weights = case PSQ.minView frontier of
        Nothing -> weights
        Just (binding, frontier') -> let
            node = PSQ.key binding
            w = PSQ.prio binding
            edges = fromJust $ Map.lookup node graph
            visited' = Set.insert node visited
            weights' = Map.insert node w weights
            frontier'' = foldr (update_keys visited' w) frontier' edges
            in
            dijkstras_helper visited' frontier'' weights'

-- acts as the inner loop of dijkstras, goes through the adjacent nodes and updates the frontier
update_keys :: Set.Set Node -> Float -> (Float, Node) -> PSQ.PSQ Node Float -> PSQ.PSQ Node Float
update_keys visited initial_weight (w, node) acc = if Set.member node visited
    then acc
    else 
         case PSQ.lookup node acc of
            Nothing -> PSQ.insert node (w + initial_weight) acc
            Just p -> PSQ.adjust (\_ -> min p (w + initial_weight)) node acc

the_us :: Graph
the_us = Map.fromList [
    ("Prov", [(2.0, "NY"), (1.0, "Boston"), (3.0, "Chester")]), 
    ("NY", [(2.7, "Boston"), (2.0, "Prov"), (5.0, "DC")]), 
    ("Boston", [(1.0, "Prov"), (2.5, "Chester")]),
    ("DC", [(5.0, "NY")]),
    ("Chester", [(3.0, "Prov"), (2.5, "Boston")])
    ]

main :: IO ()
main = do 
    putStrLn "Map of the US:"
    putStrLn $ show $ dijkstras the_us "Chester"
