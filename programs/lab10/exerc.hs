-- Lucas Martins Vasconcelos 12111BCC056
import Data.Char (toUpper)

-- exercicio 1

addPares :: [(Int, Int)] -> [Int]
addPares [] = []
addPares ((a, b) : xs) = [a + b] ++ addPares xs

-- exercicio 2

--    letra a
toUpperLista :: [Char] -> [Char]
toUpperLista [] = []
toUpperLista (a : xs) = [toUpper a] ++ toUpperLista xs

-- letra b
tuplaLista :: [Char] -> ([Char], [Char])
tuplaLista lista = (toUpperLista lista, lista)

-- exercicio 3
localizaElem :: Int -> [Int] -> Int -> Int
localizaElem _ [] cont = cont
localizaElem elem (x : xs) cont
  | x == elem = cont
  | otherwise = localizaElem elem xs (cont + 1)

maiorValor :: [Int] -> (Int, Int)
maiorValor l =
  (maior, localizaElem maior l 0)
  where
    maior = maximum l

-- exercicio 4
remove :: [Int] -> Int -> [Int]
remove lista posicao = do
  let (l1, l2) = splitAt posicao lista
  let l3 = drop 1 l2
  concat [l1, l3]

-- exercicio 5
type NomeAluno = String

type MediaNota = Int

type Aluno = (NomeAluno, MediaNota)

-- letra a
type Turma = [Aluno]

notaAluno :: Aluno -> Int
notaAluno (nome, nota) = nota

-- aprovados::Turma->[NomeAluno]
aprovados [] _ = []
aprovados (a : xs) nota = 1

main :: IO ()
main = do
  print ("soma pares")
  print (addPares [(2, 3), (2, 1), (3, 4)])
  print ("lista toUpper ")
  print (toUpperLista ['d', 'c', 'b', 'a'])
  print ("toUpper lista ")
  print (tuplaLista "dadass")
  print ("maior da lista e sua posicao")
  print (maiorValor [1, 3, 13, 4])
  print ("remove da lista pela posicao ")
  print (remove [1, 3, 13, 4] 2)
  print ("Alunos aprovados ")

-- let listaAlunos = [("Joao", 100), ("Paulo", 50), ("Maria", 55), ("Jose", 62)]
-- print (aprovados listaAlunos 60)