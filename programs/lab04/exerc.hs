import Distribution.Simple.Flag (BooleanFlag)

maior :: Int -> Int -> Int
maior a b = if a > b then a else b

menor :: Int -> Int -> Int
menor a b = if a < b then a else b

maior3 :: Int -> Int -> Int -> Int
maior3 a b c
  | a > b && a > c = a
  | b > a && b > c = b
  | otherwise = c

menor3 :: Int -> Int -> Int -> Int
menor3 a b c
  | a < b && a < c = a
  | b < a && b < c = b
  | otherwise = c

ordenaMaior3 :: Int -> Int -> Int -> (Int, Int, Int)
ordenaMaior3 a b c = (x, y, z)
  where
    x = maior3 a b c
    y = maior (menor a b) (menor b c)
    z = menor3 a b c

-- verificaMaior0 :: Int -> Bool
-- verificaMaior0 n
--   | n > 0 = True
--   | otherwise = False


-- somaN::Int -> Int
-- somaN 0 = 0
-- somaN n
--   | verificaMaior0 n = n + somaN (n - 1)
--   | otherwise = error "numero negativo"


e_par :: Int -> Bool
e_par x
  | mod x 2 == 0 = True
  | otherwise = False

fatorialDuplo :: Int -> Int
fatorialDuplo 0 = 1
fatorialDuplo 1 = 1
fatorialDuplo n
  | e_par n == True = n * fatorialDuplo (n - 2)
  | otherwise = n * fatorialDuplo (n - 2)


power::Int->Int->Int
power n 0 = 1
power n 1 = n
power 1 n = 1
power base exp = res
  where
    res = base * power base (exp - 1) 

main :: IO ()
main = do
  print ("Maior numero")
  print (maior 10 20)
  print ("Maior numero")
  print (maior 12 6)
  print ("Maior 3")
  print (maior3 12 6 2)
  print ("Ordena Maior 3")
  print (ordenaMaior3 24 32 1)
  -- print ("Soma n")
  -- print (somaN (-1))
  print ("Fatorial duplo n")
  print (fatorialDuplo 9)
  print ("potencia n")
  print (power 2 2)
