maior :: Double -> Double -> Double
maior x y = if (x > y) then x else y

menor :: Double -> Double -> Double
menor x y = if (x < y) then x else y

soma :: Double -> Double -> Double
soma x y = x + y

diferenca :: Double -> Double -> Double
diferenca x y = (maior x y) - (menor x y)

produto :: Double -> Double -> Double
produto x y = x * y

divisao :: Double -> Double -> Double
divisao x y
  | y == 0 = -1
  | otherwise = (maior x y) / (menor x y)

menu :: IO ()
menu = do
  putStrLn "1 - Soma de 2 numeros"
  putStrLn "2 - Diferenca de 2 numeros"
  putStrLn "3 - Produto de 2 numeros"
  putStrLn "4 - Divisao de 2 numeros"
  op <- getLine
  putStrLn "Digite um numero"
  x <- getLine
  putStrLn "Digite outro numero"
  y <- getLine
  case (read op) of
    1 -> putStrLn (show (soma (read x) (read y)))
    2 -> putStrLn (show (diferenca (read x) (read y)))
    3 -> putStrLn (show (produto (read x) (read y)))
    4 -> putStrLn (show (divisao (read x) (read y)))

main :: IO ()
main = do
  menu
