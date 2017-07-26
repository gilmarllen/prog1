module Grafo ((..), ) where

data Grafo = Grafo [No]
           deriving (Show)

data No = No { id :: Int, arestas :: [(Int, Double)] }
        deriving (Show)

make_grafo = Grafo []

make_no id ar = No id ar

--insert_no
