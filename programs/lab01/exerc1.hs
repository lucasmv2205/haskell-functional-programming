b array = last array

a array = init array

n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

funcLast1 array = head (reverse array)

funcLast2 array = array !! (length (array) - 1)

funcInit1 array = reverse (drop 1 (reverse array))

funcInit2 array = take (length array - 1) array

main :: IO ()
main = do
  let array = [1, 2, 3, 4, 5]
  print ("retorno da funcao com sintaxe corrigida")
  print (n)
  print ("forma 1 de implementar o last")
  print (funcLast1 array)
  print ("forma 2 de implementar o last")
  print (funcLast2 array)
  print ("forma 1 de implementar o init")
  print (funcInit1 array)
  print ("forma 2 de implementar o init")
  print (funcInit2 array)
  print ("usando o last")
  print (b array)
  print ("usando o init")
  print (a array)