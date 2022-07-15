import Distribution.Simple.Flag (BooleanFlag)

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

power :: Int -> Int -> Int
power n 0 = 1
power n 1 = n
power 1 n = 1
power base exp = res
  where
    res = base * power base (exp - 1)

resto :: (Ord t, Num t) => t -> t -> t
resto p q
  | p < q = p
  | otherwise = resto (p - q) q

quociente :: (Ord t, Num a, Num t) => t -> t -> a
quociente p q
  | p < q = 0
  | otherwise = 1 + quociente (p - q) q

nand1 :: Bool -> Bool -> Bool
nand1 p q = if (p == False || q == False) then True else False

nand2 :: Bool -> Bool -> Bool
nand2 p q
  | p == False || q == False = True
  | otherwise = False

nand3 :: Bool -> Bool -> Bool
nand3 _ False = True
nand3 False _ = True
nand3 _ _ = False

main :: IO ()
main = do
  print ("Fatorial duplo n")
  print (fatorialDuplo 9)
  print ("Resto")
  print (resto 10 3)
  print ("Quociente")
  print (quociente 10 3)
  print ("potencia n")
  print (power 2 2)
  print ("Nand if then else")
  print (nand1 False False)
  print ("Nand guardas")
  print (nand2 False False)
  print ("Nand padroes")
  print (nand3 True True)
