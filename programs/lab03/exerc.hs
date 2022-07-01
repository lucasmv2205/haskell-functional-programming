-- import GHC.Char

type Pessoa = (String, Int, Float, Char)

pess :: Int -> Pessoa
pess x
  | x == 1 = ("Cristina", 27, 1.66, 'F')
  | x == 2 = ("Flavio", 26, 1.85, 'M')
  | x == 3 = ("Mariana", 67, 1.55, 'F')
  | x == 4 = ("Cecilia", 48, 1.78, 'M')
  | x == 5 = ("Paulo", 24, 1.93, 'M')
  | x == 6 = ("Clara", 38, 1.70, 'F')
  | x == 7 = ("Rodrigo", 12, 1.85, 'M')
  | x == 8 = ("Giovana", 31, 1.58, 'F')
  | x == 9 = ("Daniel", 75, 1.74, 'M')
  | x == 10 = ("Eduardo", 21, 1.69, 'F')
  | otherwise = ("Acabou!", 0, 0.0, 'x')

altura :: Pessoa -> Float
altura (a, b, c, d) = c

maiorAltura :: Int -> Int -> Int
maiorAltura x y
  | altura (pess x) > altura (pess y) = x
  | otherwise = y

menorAltura :: Int -> Int -> Int
menorAltura x y
  | altura (pess x) < altura (pess y) = x
  | otherwise = y

diferencaAltura :: Int -> Int -> Float
diferencaAltura x y = altura (pess (maiorAltura x y)) - altura (pess (menorAltura x y))

pessoaMaisAlta::Int->Pessoa
pessoaMaisAlta x = pess (maiorAlturaRecursiva 0.0 x 0)

maiorAlturaRecursiva::Float->Int->Int->Int
maiorAlturaRecursiva maior x posmaior
    | x == 1 = posmaior
    | altx > maior = maiorAlturaRecursiva altx (x-1) x
    | otherwise = maiorAlturaRecursiva maior (x-1) posmaior
    where 
      altx  = altura(pess x)

valorConvertido :: Double -> ((Double, String), (Double, String), (Double, String))
valorConvertido x = ((x, "real"), (x * 0.20, "euro"), (x * 0.21, "dolar"))



main :: IO ()
main = do
  print ("Maior altura")
  print (maiorAltura 4 4 - 1)
  print ("Diferenca altura")
  print (diferencaAltura 4 5)
  print ("Valor convertido")
  print (valorConvertido 500.80)
  print ("Pessoa mais alta")
  print (pessoaMaisAlta 10)