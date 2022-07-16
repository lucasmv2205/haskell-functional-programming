-- Lucas Martins Vasconcelos - 12111BCC056

eLogico :: Bool -> Bool -> Bool
eLogico p q
  | p == True && q == True = True
  | otherwise = False

eLogico2 :: Bool -> Bool -> Bool
eLogico2 p q
  | p == True = q
  | otherwise = False

comb n 1 = n
comb n k
  | n == k = 1
  | otherwise = comb (n - 1) (k - 1) + comb (n - 1) k

imprimeNvezes :: Int -> IO ()
imprimeNvezes 0 = putStrLn "Fim"
imprimeNvezes n = do
  putStrLn "Frase"
  imprimeNvezes (n - 1)

main :: IO ()
main = do
  print ("e logico guardas")
  print (eLogico False True)
  print ("e logico2 guardas")
  print (eLogico2 False True)
  print ("Combo")
  print (comb 4 2)
  print ("Combo")
  print (comb 4 2)
  print ("Imprime")
  imprimeNvezes 4