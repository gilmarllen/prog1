module Arvorebb (Arv (..), make_tree, insert_tree, find_tree, enlista) where

--Arvore Binaria de Busca sem elementos repetidos

data Arv a = Nulo
           | No (Arv a) a (Arv a)
           deriving Show

make_tree = Nulo

insert_tree ele Nulo = No Nulo ele Nulo
insert_tree ele (No e atual d) | ele == atual = (No e atual d)
                               | ele > atual = (No e atual (insert_tree ele d))
                               | otherwise = (No (insert_tree ele e) atual d)

find_tree ele Nulo = False
find_tree ele (No e atual d) | ele == atual = True
                             | ele > atual = find_tree ele d
                             | otherwise = find_tree ele e

enlista Nulo = []
enlista (No e atual d) = (enlista e) ++ [atual] ++ (enlista d)
