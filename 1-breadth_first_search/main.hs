import Data.Graph.Inductive.Graph
    ( Node 
    , LNode
    , Edge
    )
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.Graph.Inductive.PatriciaTree as PG
import qualified Data.Graph.Inductive.Dot          as GD

import Control.Monad (guard)
import Data.Functor ((<&>))
import System.Environment
import Data.List (foldl')
import System.Process (system)

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
    args <- getArgs
    guard ((not . null) args)
    let graphFile = head args
    fileContents <- readFile graphFile <&> lines
    let (numNodes, graphEdges) = parseEdges fileContents 
        g = buildGraph numNodes graphEdges
    G.prettyPrint g
    writeDot "example" g
    print (bfs 0 g)

buildGraph :: Int -> [Edge] -> PG.Gr Int Int
buildGraph x xs = G.insEdges (labelEdges xs) justNodesGraph
    where
        justNodesGraph = G.insNodes ((\a -> (a, a)) <$> [0..x - 1]) G.empty
        labelEdges ys = fmap (\((a, b), c) -> (a, b, c)) (zip ys [1..])

parseEdges :: [String] -> (Int, [Edge])
parseEdges []     = (0, [])
parseEdges (x:xs) = (read x, listToTuple <$> parseEdges xs)
    where parseEdges = foldl' (\acc z -> parseEdgeStr z : acc) []
          parseEdgeStr ys = strToList ys
          strToList = (\zs -> read <$> zs) <$> words
          listToTuple []      = error errorMsg
          listToTuple [a]     = error errorMsg
          listToTuple (a:b:_) = (a, b)
          errorMsg = "List needs two contain at least two elements"

writeDot :: (Show a, Show b, G.Graph gr) => String -> gr a b -> IO ()
writeDot fileName graph = do 
    let dot = GD.showDot (GD.fglToDot graph)
    writeFile (fileName ++ ".dot") dot
    _ <- system("dot -Tpng -o" ++ fileName ++ ".png " ++ fileName ++ ".dot")
    return ()
