-- 01
import Data.Char
import Distribution.Simple.Flag (BooleanFlag)

converte :: Char -> (Char, Char, Int)
converte a
  | isUpper a == True = (a, toLower a, ord a)
  | otherwise = (a, toUpper a, ord a)

-- 02

calculaBaskara :: (Float, Float, Float) -> (Float, Float)
calculaBaskara (a, b, c)
  | calculaDelta < 0 = error "RAIZES IMAGINARIAS"
  | otherwise = calculaRaizes
  where
    calculaDelta = (b * b) - (4 * a * c)
    calculaRaizes = (((-b) + sqrt calculaDelta) / (2 * a), ((-b) - sqrt calculaDelta) / (2 * a))

-- 03
type Nome = String

type Idade = Double

type Sexo = String

type Pessoa = (Nome, Idade, Sexo)

pessoa :: Double -> Pessoa
pessoa 1 = ("Lucas", 22, "M")
pessoa 2 = ("Millena", 20, "F")
pessoa 3 = ("Julia", 25, "F")
pessoa 4 = ("Joao", 26, "M")

pessoa2 :: Double -> Pessoa
pessoa2 id
  | id == 1 = ("Lucas", 22, "M")
  | id == 2 = ("Millena", 20, "F")
  | id == 3 = ("Julia", 25, "F")
  | id == 4 = ("Joao", 26, "M")

idade :: Pessoa -> Double
idade (nome, idade, sexo) = idade

somaIdades :: Double -> Double
somaIdades 1 = idade (pessoa 1)
somaIdades total = idade (pessoa total) + somaIdades (total - 1)

mediaIdades :: Double -> Double
mediaIdades x = somaIdades x / x

main :: IO ()
main = do
  print ("Soma idades")
  print (somaIdades 4)
  print (mediaIdades 4)
