module Graphs.Filesystem where

import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.Graph.Inductive.PatriciaTree as PG
import qualified Data.Graph.Inductive.Dot          as GD
import Data.Graph.Inductive.Graph (Edge)
import Control.Monad (guard)
import Data.Functor ((<&>))
import System.Process (system)
import Data.List (foldl')
import System.Environment (getArgs)

data EnvData a = EnvData { filename :: String, returnData :: a }

getEdgesFromArgs :: IO (EnvData (Int, [Edge]))
getEdgesFromArgs = do 
    args <- getArgs 
    guard ((not . null) args)
    let graphFile = head args 
    fileContents <- readFile graphFile <&> lines
    return $ EnvData graphFile (parseEdges fileContents)

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
