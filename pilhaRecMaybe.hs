module Pilha (Pilha (..), make_pilha, top, push, pop, empty) where

data Pilha p = Vazia
             | Elemento p (Pilha p)
             deriving (Show)

make_pilha = Vazia

top (Vazia) = Nothing
top (Elemento e p) = Just e

push ele (Vazia) = Elemento ele Vazia
push ele p = Elemento ele p

pop (Vazia) = (Nothing, Vazia)
pop (Elemento e p) = (Just e, p)

empty (Vazia) = True
empty p = False
