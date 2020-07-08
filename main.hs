module Main where
import Dijkstra
import System.IO

main = do
    graphString <- readFile "./graph.txt"
    let graph = graphConverter graphString
        path = pack graph "Ankara" "Gaziantep" -- The shortest path between Ankara and Gaziantep
    print(path)