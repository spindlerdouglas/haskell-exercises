module Graphs where

type Vertex = Int 
type Edge = (Vertex, Vertex)
type Graph = [Vertex]


graph :: [Edge]
graph = [(1,2), (1,3), (1,4), (1,5),
         (2,6), (2,7), (4,8), (5,9)]	

getAdjacent :: Graph -> Vertex -> [Vertex]
getAdjacent [] _ = []
getAdjacent ((a,b):c) v 
						| (a == v) = b:(getAdjacent c v)
						| (b == v) = a:(getAdjacent c v)
						| otherwise = getAdjacent c v