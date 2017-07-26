module FilaP (FilaP (..), make_fila, front, push, pop, empty) where

--Maybe apenas nos retornos, nao na estrutura
--Elementos Menores tem Prioridade na Fila

data FilaP f = FilaP [f]
               deriving (Show)

make_fila = FilaP []

front (FilaP []) = Nothing
front (FilaP l) = Just $ head l


push ele (FilaP []) = FilaP [ele]
push ele (FilaP l) | (head l) > ele = FilaP (ele : l)
                   | otherwise = push (head l) ( push ele (FilaP (tail l)) )

pop (FilaP []) = (Nothing, FilaP [])
pop (FilaP l) = (Just (head l), FilaP $ tail l)

empty (FilaP []) = True
empty (FilaP l) = False
