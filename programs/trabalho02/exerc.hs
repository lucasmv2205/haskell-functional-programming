-- Lucas Martins Vasconcelos - 12111BCC056

type Codigo = Int

type Nome = String

type Preco = Float

type Produto = (Codigo, Nome, Preco)

tabelaProdutos :: [Produto]
tabelaProdutos =
  [ (001, "Chocolate", 5.20),
    (002, "Biscoito", 4.25),
    (003, "Laranja", 7.25),
    (004, "Sabao", 2.25),
    (005, "Doritos", 8.25)
  ]

getCodigo :: Produto -> Codigo
getCodigo (codigo, nome, preco) = codigo

getNome :: Produto -> Nome
getNome (codigo, nome, preco) = nome

getPreco :: Produto -> Preco
getPreco (codigo, nome, preco) = preco

isCodigo :: Codigo -> Produto -> Bool
isCodigo cod (codigo, nome, preco) = if cod == codigo then True else False

buscaPrecoPorCodigo :: Codigo -> Preco
buscaPrecoPorCodigo codigo = getPreco (head (filter (isCodigo codigo) tabelaProdutos))

buscaNomePorCodigo :: Codigo -> Nome
buscaNomePorCodigo codigo = getNome (head (filter (isCodigo codigo) tabelaProdutos))

calculaPrecos :: [Codigo] -> Float
calculaPrecos lista = sum (map buscaPrecoPorCodigo lista)

formataStrProduto :: Codigo -> String
formataStrProduto codigo = do
  let nomeProduto = buscaNomePorCodigo codigo
  let precoProduto = buscaPrecoPorCodigo codigo
  let tamanho = 30 - (length nomeProduto + length (show precoProduto))
  nomeProduto ++ replicate tamanho '.' ++ show precoProduto ++ "\n"

formataStrTotal :: Float -> String
formataStrTotal total = do
  let tamanho = 30 - (length "Total:" + length (show total))
  "total:" ++ replicate tamanho '.' ++ show total ++ "\n"

geraNotaFiscal :: [Codigo] -> IO ()
geraNotaFiscal lista = do
  writeFile "notaFiscal.txt" (foldr (++) "" (map formataStrProduto lista))
  let total = calculaPrecos lista
  appendFile "notaFiscal.txt" (formataStrTotal total)

main :: IO ()
main = do
  print ("teste")
  geraNotaFiscal [001, 002]