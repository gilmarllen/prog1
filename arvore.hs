module arvore (Arv (No, Folha), enlista) where

data Arv = Folha a
         | No (Arv a) a (Arv a)
         deriving Show

enlista (Folha a) = a
enlista (No e m d) = 
