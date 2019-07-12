module BaseGraph (
    -- types
    BaseGraph,
    Vertex,
    Edge,

    -- graph create
    createBaseGraph,

    -- graph update
     addEdge,
     removeEdge,
     addVertex,

    -- graph visualization
--     drawGraph,

    -- graph characteristics
    getEdges,
    getEdgeCount,
    getVertices,
    getVertexCount,
    getAllPaths,
    getPaths,
    findFirstNeighbors,

    -- graph algorithms
    depthFirst,
    getConnectedComponents


) where

import Data.Array
import Data.List
import Control.Monad (guard)

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
removeElem x (y:ys) | x == y = removeElem x ys
                    | otherwise = y : removeElem x ys

incrementVertexInterval :: BaseGraph -> Edge
incrementVertexInterval _graph = (minimum (getVertices _graph), maximum (getVertices _graph) + 1)

addVertex :: BaseGraph -> BaseGraph
addVertex _graph = createBaseGraph (incrementVertexInterval _graph) (getEdges _graph)


findFirstNeighbors :: BaseGraph -> Vertex -> [Vertex]
findFirstNeighbors _graph _vertex = snd $ unzip $ filter (\(u,_) -> u == _vertex) (getEdges _graph)

depthFirst :: BaseGraph -> Vertex -> [Vertex]
depthFirst _graph _vertex = depthFirst' (getVertices _graph, getEdges _graph) _vertex

depthFirst' :: ([Vertex], [Edge]) -> Vertex -> [Vertex]
depthFirst' (_vertices, _edges) _vertex
    | [ x | x <- _vertices, x == _vertex] == [] = []
    | otherwise = dfRecursive (_vertices, _edges) [_vertex]

dfRecursive :: ([Vertex], [Edge]) -> [Vertex] -> [Vertex]
dfRecursive ([],_) _ = []
dfRecursive (_,_) [] = []
dfRecursive (_vertices, _edges) (top:_stack)
    | [x | x <- _vertices, x == top] == [] = dfRecursive (newv, _edges) _stack
    | otherwise = top : dfRecursive (newv, _edges) (adjacent ++ _stack)
    where
        adjacent = [x | (x,y) <- _edges, y == top] ++ [x | (y,x) <-_edges, y == top]
        newv = [x | x <- _vertices, x /= top]

getConnectedComponents :: BaseGraph -> [[Vertex]]
getConnectedComponents _graph = getConnectedComponents' (getVertices _graph, getEdges _graph)

getConnectedComponents' :: ([Vertex], [Edge]) -> [[Vertex]]
getConnectedComponents' ([],_) = []
getConnectedComponents' (top:_vertices, _edges)
    | remaining == [] = [connected]
    | otherwise = connected : getConnectedComponents' (remaining, _edges)
    where
        connected = depthFirst' (top:_vertices, _edges) top
        remaining = (top:_vertices) \\ connected



