module BaseGraph (
    -- types
    BaseGraph,
    Vertex,
    Edge,

    -- graph create
    createBaseGraph,
--     generateRandomBaseGraph,

    -- graph update
--     addEdge,
--     removeEdge,
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

type Vertex = Int

type BaseGraph = Array Vertex [Vertex]

type Edge = (Vertex, Vertex)

createBaseGraph :: Edge -> [Edge] -> BaseGraph
createBaseGraph _interval _edges = accumArray (flip (:)) [] _interval _edges

getEdges :: BaseGraph -> [Edge]
getEdges _graph = [ (u, v) | u <- (getVertices _graph), v <- _graph!u ]

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
                  _start:path | edge <-_edges, (fst edge) == _start,
                  path <- (getPaths (snd edge) _end [e | e <- _edges, e /= edge])
             ]


