module PilhaL (PilhaL (..), make_pilha, top, push, pop, empty) where

data PilhaL p = PilhaL [p]
                deriving (Show)

make_pilha = PilhaL []

top (PilhaL []) = Nothing
top (PilhaL p) = Just $ head p

push ele (PilhaL p) = PilhaL $ ele : p

pop (PilhaL []) = (Nothing, (PilhaL []))
pop (PilhaL p) = (Just $ head p, (PilhaL $ tail p))

empty (PilhaL []) = True
empty (PilhaL p) = False
