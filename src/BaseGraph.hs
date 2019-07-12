module BaseGraph (
    -- types
    BaseGraph,
    Vertex,
    Edge,

    -- graph create
    createBaseGraph,
--     generateRandomBaseGraph,

    -- graph update
     addEdge,
     removeEdge,
--     addVertex,
--     removeVertex,

    -- graph visualization
--     drawGraph,

    -- graph characteristics
    getEdges,
    getEdgeCount,
    getVertices,
    getVertexCount,
    getAllPaths,
    getPaths,
--     getDeg,
--
--     -- graph algorithms
--     dfs,
--     bfs


) where

import Data.Array
import Data.List

type Vertex = Int

type BaseGraph = Array Vertex [Vertex]

type Edge = (Vertex, Vertex)

createBaseGraph :: Edge -> [Edge] -> BaseGraph
createBaseGraph = accumArray (flip (:)) []

getEdges :: BaseGraph -> [Edge]
getEdges _graph = [ (u, v) | u <- getVertices _graph, v <- _graph!u ]

getEdgeCount :: BaseGraph -> Int
getEdgeCount = length.getEdges

getVertices :: BaseGraph -> [Vertex]
getVertices = indices

getVertexCount :: BaseGraph -> Int
getVertexCount = length.getVertices

getAllPaths :: Vertex -> Vertex -> BaseGraph -> [[Vertex]]
getAllPaths _start _end _graph = getPaths _start _end (getEdges _graph)

getPaths :: Vertex -> Vertex -> [Edge] -> [[Vertex]]
getPaths _start _end _edges
             | _start == _end = [[_end]]
             | otherwise = [
                  _start:path | edge <-_edges, fst edge == _start,
                  path <- getPaths (snd edge) _end [e | e <- _edges, e /= edge]
             ]

getVertexInterval :: BaseGraph -> Edge
getVertexInterval _graph = (minimum (getVertices _graph), maximum (getVertices _graph))

addEdge :: Edge -> BaseGraph -> BaseGraph
addEdge _edge _graph = createBaseGraph (getVertexInterval _graph) ((getEdges _graph) ++ [_edge])

removeEdge :: Edge -> BaseGraph -> BaseGraph
removeEdge _edge _graph = createBaseGraph (getVertexInterval _graph) (removeElem _edge (getEdges _graph))

removeElem _ [] = []
removeElem x (y:ys) | x == y    = removeElem x ys
                    | otherwise = y : removeElem x ys




