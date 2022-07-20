-- Lucas Martins Vasconcelos - 12111BCC056

module Main (main) where

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

soma x y = x + y

diferenca x y = x - y

imprime :: String -> IO ()
imprime nome_arquivo = do
  conteudo <- readFile nome_arquivo
  putStrLn "\nValor: "
  putStrLn conteudo
  putStrLn "\nFim do arquivo"

faz_deposito = do
  putStrLn "Qual valor deseja depositar: "
  valor <- readLn
  deposito valor

deposito :: Float -> IO ()
deposito valor = do
  conteudo <- readFile "saldo.txt"
  let total = soma (read conteudo) valor
  let totalS = show total
  putStrLn ("\nSaldo atual " ++ totalS)
  writeFile "saldo.txt" totalS
  appendFile "extrato.txt" ("\n+valorDeposito " ++ show valor)

faz_saque = do
  putStrLn "Qual valor deseja sacar: "
  valor <- readLn
  saca valor

saca :: Float -> IO ()
saca valor = do
  conteudo <- readFile "saldo.txt"
  let total = diferenca (read conteudo) valor
  let totalS = show total
  putStrLn ("\nSaldo atual " ++ totalS)
  writeFile "saldo.txt" totalS
  appendFile "extrato.txt" ("\n-valorSaque " ++ show valor)

menu :: IO ()
menu = do
  hSetBuffering stdout NoBuffering
  putStrLn "=============================="
  putStrLn "Banco Lucas Martins Vasconcelos"
  putStrLn "=============================="
  putStrLn "Opcoes"
  putStrLn "1 - Saldo"
  putStrLn "2 - Extrato"
  putStrLn "3 - Deposito"
  putStrLn "4 - Saque"
  putStrLn "5 - Fim"
  op <- readLn
  case (op) of
    1 -> imprime "saldo.txt"
    2 -> imprime "extrato.txt"
    3 -> faz_deposito
    4 -> faz_saque
    5 -> putStrLn "Obrigado por usar o banco"
    _ -> putStrLn "Opcao invalida"
  if not (op == 5) then main else putStrLn "Saindo..."

main :: IO ()
main = do
  menu
