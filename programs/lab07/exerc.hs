ordena2 :: Int -> Int -> (Int, Int)
ordena2 x y = if x > y then (y, x) else (x, y)

ordena2g :: Int -> Int -> (Int, Int)
ordena2g x y
  | x > y = (y, x)
  | otherwise = (x, y)

numMes :: Int -> String
numMes n
  | n == 1 = "janeiro"
  | n == 2 = "fevereiro"
  | n == 3 = "marco"
  | n == 4 = "abril"
  | n == 5 = "maio"
  | n == 6 = "junho"
  | n == 7 = "julho"
  | n == 8 = "agosto"
  | n == 9 = "setembro"
  | n == 10 = "outubro"
  | n == 11 = "novembro"
  | n == 12 = "dezembro"
  | otherwise = "Erro"

numMesCase :: Int -> String
numMesCase x =
  case x of
    1 -> "janeiro"
    2 -> "fevereiro"
    3 -> "marco"
    4 -> "abril"
    5 -> "maio"
    6 -> "junho"
    7 -> "julho"
    8 -> "agosto"
    9 -> "setembro"
    10 -> "outubro"
    11 -> "novembro"
    12 -> "dezembro"
    _ -> "Erro"

func :: Int -> Int -> Int
func x y =
  let somaDosQuadrados = x * x + y * y
      quadradoDaSoma = (x + y) * (x + y)
   in if somaDosQuadrados > quadradoDaSoma then somaDosQuadrados else quadradoDaSoma

isTriangulo :: Float -> Float -> Float -> Bool
isTriangulo a b c
  | a < (b + c) && b < (a + c) && c < (a + b) = True
  | otherwise = False

classificacaoTriangulo :: Float -> Float -> Float -> String
classificacaoTriangulo x y z
  | x == y && x == z && isTriangulo x y z = "Equilatero"
  | (x == y || x == z || y == z) && isTriangulo x y z = "Isosceles"
  | x /= y && y /= z && isTriangulo x y z = "Escaleno"
  | otherwise = "Nao forma um triangulo"

menuOpcao :: IO ()
menuOpcao = do
  putStrLn "1 - Digite um nome"
  nome <- getLine
  putStrLn "2 - Digite um matricula"
  matricula <- getLine
  putStrLn "3 - Digite um nota"
  nota <- getLine
  print ("Nome: " ++ nome ++ "; " ++ "Matricula: " ++ matricula ++ "; " ++ "Nota: " ++ nota)

main :: IO ()
main = do
  print ("Ordena2")
  print (ordena2 6 2)
  print (ordena2g 6 2)
  print (numMes 13)
  print (numMesCase 5)
  print (func 5 4)
  print (func 2 2)
  print (classificacaoTriangulo 2 2 25)
  print (classificacaoTriangulo 3 4 5)
  menuOpcao
