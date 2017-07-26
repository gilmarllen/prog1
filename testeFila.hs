import Fila

constroi_fila [] fl = fl
constroi_fila (x:xs) fl = (constroi_fila xs fl_prox)
                        where fl_prox = (push (read x ::Int) fl)

main = do arquivo <- readFile "arq"
          let elementos = lines arquivo
          let ini = make_fila
          let f = constroi_fila elementos ini
          print f
          print("Removendo 3 elementos")
          let g = pop f
          let f = pop $ snd g
          let g = pop $ snd f
          print g
          putStr "Front: "
          print $ front $ snd g
          putStr "Empty? "
          print $ empty $ snd g
