module Main (main) where

import Data.Graph.Inductive.Graph
    ( Node 
    , LNode
    , Edge
    )
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.Graph.Inductive.PatriciaTree as PG
import qualified Data.Graph.Inductive.Dot          as GD

import Graphs.Filesystem (EnvData (..), getEdgesFromArgs, writeDot)

bfs :: (Eq a, G.Graph gr) => Node -> gr a b -> [Node]
bfs src = bfs' [src]

-- TODO: Convert this to a tree instead of just a list and also, use 
-- a proper queue data structure with an amortized runtime of O(1)
bfs' :: (Eq a, G.Graph gr) => [Node] -> gr a b -> [Node]
bfs' []     _ = []
bfs' (x:xs) g = case G.match x g of
    (Nothing, g') -> bfs' xs g'
    (Just x', g') -> (G.node' x') : bfs' (xs ++ (G.neighbors g x)) g'

main :: IO ()
main = do 
    envdata <- getEdgesFromArgs
    let (EnvData filename (numNodes, graphEdges)) = envdata
        g = buildGraph numNodes graphEdges 
    G.prettyPrint g 
    writeDot filename g 
    print (bfs 0 g)

buildGraph :: Int -> [Edge] -> PG.Gr Int Int
buildGraph x xs = G.insEdges (labelEdges xs) justNodesGraph
    where
        justNodesGraph = G.insNodes ((\a -> (a, a)) <$> [0..x - 1]) G.empty
        labelEdges ys = fmap (\((a, b), c) -> (a, b, c)) (zip ys [1..])


