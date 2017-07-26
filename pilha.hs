module Pilha (Pilha (..), top) where

data Pilha p = Vazia
             | Elemento p (Pilha p)
             deriving (Show)

make_pilha = Vazia

top (Vazia) = Nothing
top (Elemento e p) = Just e

push ele (Vazia) =
