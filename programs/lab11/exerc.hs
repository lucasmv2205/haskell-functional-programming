-- Lucas Martins Vasconcelos  - 12111BCC056

import Data.Char (isDigit)

-- exercicio 1

-- letra a
numof :: Char -> String -> Int
numof x list = length (filter (== x) list)

-- letra b
ellen :: Foldable t => [t a] -> [Int]
ellen list = map length list

-- letra c
ssp :: (Floating b, Ord b) => [b] -> b
ssp list = foldr (+) 0 (map (** 2) (filter (> 0) list))

-- exercicio 2

separa :: [Char] -> ([Char], [Char])
separa lista = (filter isDigit lista, filter (not . isDigit) lista)

-- exercicio 3

-- letra a
-- a funcao const retorna, de dois valores que foram inseridos pelo usuario, o primeiro valor que foi inserido
const_novo :: p1 -> p2 -> p1
const_novo x y = x
--nome da funcao trocado pois o prelude ja possui uma funcao built-in chamada const

-- letra b
--a funcao swap, a partir de uma tupla inserida, retorna essa mesma dupla invertida
swap :: (b, a) -> (a, b)
swap (x, y) = (y, x)

-- letra c
--a funcao apply, a partir de uma funcao e um valor inseridos, retorna o retorno da funcao inserida aplicada ao valor inserido
apply :: (t1 -> t2) -> t1 -> t2
apply f x = f x

--letra d
--a funcao flip, a partir de uma funcao e dois valores inseridos, retorna o retorno da funcao inserida aplicada aos valores em ordem invertida
--(o segundo elemento inserido sera o primeiro parametro da funcao e o primeiro elemento inserido sera o segundo parametro da funcao)
flip_novo :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip_novo f x y = f y x
--nome da funcao trocado pois o prelude ja possui uma funcao built-in chamada flip

-- exericio 4

-- nome, quantidade e preço unitário de um item
--type ShopItem = (String, Float, Float)

data ShopItem = ShopItem String Float Float

selec_preco :: ShopItem -> Float
selec_preco (ShopItem nome qty preco) = preco*qty

preco_total :: [ShopItem] -> Float
preco_total lista = sum (map(selec_preco) lista)

main :: IO ()

main = do
  print ("numof")
  print (numof 'a' "aaaaabbbbbbb")
  print ("ellen")
  print (ellen ["bbb", "bbbaaagggg"])
  print ("ssp")
  print (ssp [1, 2, 3, 4])
  print ("separa")
  print (separa "asadas312321")
  print ("preco total")
  let lista_itens = [(ShopItem "nome 1" 1 2.50), (ShopItem "nome 2" 2 3)]
  print (preco_total lista_itens)
