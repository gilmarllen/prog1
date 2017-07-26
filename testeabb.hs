import Arvorebb

make_abb [] fl = fl
make_abb (x:xs) fl = (make_abb xs fl_prox)
                        where fl_prox = (insert_tree (read x ::Int) fl)

main = do arquivo <- readFile "arq"
          let elementos = lines arquivo
          let ini = make_tree
          let arv = make_abb elementos ini
          print arv
          print $ find_tree 15 arv
          print $ find_tree 16 arv
          print $ enlista arv
