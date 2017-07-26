module Fila (Fila (..), make_fila, front, push, pop, empty) where

--Maybe apenas nos retornos, nao na estrutura

data Fila f = Vazia
             | Elemento f (Fila f)
             deriving (Show)

make_fila = Vazia

front (Vazia) = Nothing
front (Elemento e f) = Just e

push ele Vazia = Elemento (ele) (Vazia)
push ele (Elemento e f) = Elemento (e) (push (ele) (f))

pop (Vazia) = (Nothing, Vazia)
pop (Elemento e f) = (Just e, f)

empty Vazia = True
empty (Elemento e f) = False
