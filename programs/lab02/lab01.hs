funcDobro number = 2 * number

funcQuadruplica number = funcDobro (funcDobro number)

funcSoma2 x y = x + y

funcPow x y = x ** y

funcSoma4 x y z w = funcSoma2 x y + funcSoma2 z w

misterio x y z w = funcSoma2 (funcSoma2 x y) (funcSoma2 z w)

funcHipotenusa x y = sqrt ((funcPow x 2) + (funcPow y 2))

main :: IO ()
main = do
  print ("Dobro")
  print (funcDobro 2)
  print ("Quadruplo")
  print (funcQuadruplica 2)
  print ("Soma de 2")
  print (funcSoma2 2 3)
  print ("Soma de 4")
  print (funcSoma4 2 3 2 3)
  print ("Misterio")
  print (misterio 2 3 2 3)
  print ("Pow")
  print (funcPow 2 3)
  print ("Hipotenusa")
  print (funcHipotenusa 3 4)