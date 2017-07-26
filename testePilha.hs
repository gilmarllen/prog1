import Pilha

constroi_pilha [] fl = fl
constroi_pilha (x:xs) fl = (constroi_pilha xs fl_prox)
                        where fl_prox = (push (read x ::Int) fl)

main = do arquivo <- readFile "arq"
          let elementos = lines arquivo
          let ini = make_pilha
          let f = constroi_pilha elementos ini
          print f
          print("Removendo 3 elementos")
          let g = pop f
          let f = pop $ snd g
          let g = pop $ snd f
          print g
          putStr "Top: "
          print $ top $ snd g
          putStr "Empty? "
          print $ empty $ snd g
