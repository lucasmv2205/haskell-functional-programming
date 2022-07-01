subtracao :: Double -> Double -> Double
subtracao x y = x - y

areaCirculo :: Double -> Double
areaCirculo raio = raio * raio * pi

diferencaAreaCirculo :: Double -> Double -> Double
diferencaAreaCirculo raio1 raio2 = subtracao (areaCirculo raio1) (areaCirculo raio2)

logica :: Bool -> Bool -> Bool
logica p q = (p || q) && not (p && q)

main :: IO ()
main = do
  print ("Subtracao")
  print (subtracao 6 2)
  print ("Area circulo")
  print (areaCirculo 6)
  print ("Diferenca Area circulo")
  print (diferencaAreaCirculo 8 6)
  print ("funcao logica")
  print (logica True False)