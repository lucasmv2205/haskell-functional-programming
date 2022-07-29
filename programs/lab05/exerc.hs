import Data.Char (ord, toUpper, isLower)

somaLista :: [Float] -> Float
somaLista [] = 0.0
somaLista (x : xs) = x + somaLista xs

tamanhoLista [] = 0.0
tamanhoLista (x : xs) = 1 + tamanhoLista xs

e_par :: Int -> Bool
e_par x
  | mod x 2 == 0 = True
  | otherwise = False

tamanhoListaPar :: [Int] -> Int
tamanhoListaPar [] = 0
tamanhoListaPar [x]
  | e_par x == True = 1
  | otherwise = 0
tamanhoListaPar (x : xs)
  | e_par x == True = 1 + tamanhoListaPar xs
  | otherwise = tamanhoListaPar xs

-- maiuscula :: String -> String
-- maiuscula [] = ""
-- maiuscula (x : xs) = [toUpper x] ++ maiuscula xs

maiuscula2 :: String -> String
maiuscula2 [] = []
maiuscula2 (x : xs) 
  | isLower x = toUpper x : maiuscula2 xs
  | otherwise =  x : maiuscula2 xs

appendElement x element = x ++ [element]

pertence [] element = False
pertence (x: xs) element
  | x == element = True
  | otherwise = pertence xs element

calculaMedia listaInt = somaLista listaInt / tamanhoLista listaInt

maior [a] = a
maior (a: t) = if (a > (maior t)) then a else (maior t)

main :: IO ()
main = do
  print ("soma lista")
  print (somaLista [1, 2, 3, 4, 5, 6])
  print ("Numero de pares")
  print (tamanhoListaPar [1, 2, 3, 4, 8])
  print ("uppercase")
  print (maiuscula2 "aaaaa")
  print ("append element")
  print (appendElement ['a','a','a','a'] 'b')
  print ("pertence")
  print (pertence ['a','a','a','a'] 'b')
  print ("media soma de elementos")
  print (calculaMedia [4,5,2,7])