module Dijkstra(
    graphConverter,
    pack
) where

import Data.List
import System.IO

data Edge = Edge {node::Node, weight::Float} deriving (Show)
type Node = String
type Graph = [(Node, [Edge])]
type Node_ = (Node, (Float, Node))

edgesOf :: Graph -> Node -> [Edge]
edgesOf graph aNode = snd . head . filter(\(nd, _) -> nd == aNode) $ graph

elementConverter :: [((String , String), Float)] -> Graph
elementConverter elements =
    let nodes = nub . map (fst . fst) $ elements
        edgesOf elements node =                 
            let connected = filter (\((n, _), _) -> node == n) $ elements
            in map (\((_, n), weight) -> Edge n weight) connected        
    in map (\n -> (n, edgesOf elements n)) nodes


addReversedEdges :: [((String, String), Float)] -> [((String , String), Float)]
addReversedEdges elements = elements ++ map (\((n1, n2), w) -> ((n2, n1), w)) elements

graphConverter :: String -> Graph
graphConverter string =
    let readElement[n1, n2, w] = ((n1, n2), read w :: Float)
        elements = map (readElement . words) $ lines string
    in elementConverter $ addReversedEdges elements          

weightOf :: Node -> [Edge] -> Float
weightOf n = weight . head . filter (\e -> n == node e)

nodesOfEdges :: [Edge] -> [Node]
nodesOfEdges = map node

node_Init :: Graph -> Node -> [Node_]
node_Init graph startNode =
    let distanceInit (n, edges) =                      
            if n == startNode                          
            then 0
            else if startNode `elem` nodesOfEdges edges
                then weightOf startNode edges          
                else 1e10                              
    in map (\graphElement@(n,_) -> (n, ((distanceInit graphElement), startNode))) graph


node_Update :: Node_ -> Node_ -> [Node] -> [Edge] -> Node_
node_Update dn@(n, (nd, p)) (c, (cd, _)) cnodes edges =
    let w = weightOf n edges
    in  if n `notElem` cnodes then dn
        else if cd+w < nd then (n, (cd+w, c)) else dn

dijkstraRecursive :: Graph -> [Node_] -> [Node] -> [Node_]
dijkstraRecursive graph nodes_ [] = nodes_
dijkstraRecursive graph nodes_ nodesRemaining =
    let nodes_Remaining = filter (\dn -> (fst dn) `elem` nodesRemaining) nodes_
        shortestnode_ = head . sortBy (\(_,(distance1, _)) (_,(distance2,_)) -> compare distance1 distance2) $ nodes_Remaining
        shortestTarget = fst shortestnode_
        newNodesRemaining = delete shortestTarget nodesRemaining
        edges = edgesOf graph shortestTarget
        cnodes = intersect (nodesOfEdges edges) newNodesRemaining
        newnodes_ = map (\dn -> node_Update dn shortestnode_ cnodes edges) nodes_
    in dijkstraRecursive graph newnodes_ newNodesRemaining

dijkstra :: Graph -> Node -> [Node_]
dijkstra graph startNode =
    let nodes_ = node_Init graph startNode
        nodesRemaining = map fst nodes_
        in dijkstraRecursive graph nodes_ nodesRemaining


nodeMatchnode_ :: [Node_] -> Node -> Node_
nodeMatchnode_ nodes_ n = head . filter (\(x, _) -> x == n) $ nodes_


result :: [Node_] -> Node -> [Node]
result nodes_ destinationNode =
    let dn@(n, (d, p )) = nodeMatchnode_ nodes_ destinationNode
    in if n == p then [n] else result nodes_ p ++ [n]


pack :: Graph -> Node -> Node -> [Node]
pack graph startNode destinationNode =
    let so = dijkstra graph startNode
    in result so destinationNode
    