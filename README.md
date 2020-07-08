# haskell-dijkstra

The informations of the graph are in graf.txt, with [City1] [City2] [Distance] format. 
You can run main.hs file to see the shortest path between Ankara and Gaziantep nodes.
If you want to find out the shortest path between different nodes you can simply change
```diff
-   path = pack graph "Ankara" "Gaziantep"
```

this code in main.hs file. Only change node names.
