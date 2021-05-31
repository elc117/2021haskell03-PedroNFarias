--Adiciona 10 aos valores da lista
add10toall :: [Int] -> [Int]
add10toall list = [x+10 | x <- list]

--Recebe um valor e uma lista e multiplica os valores da lista pelo valor recebido
multN :: Int -> [Int] -> [Int]
multN n list = [x*n | x <- list]

--Recebe um valor e uma lista e multiplica os valores da lista pelo valor recebido
multN' :: Int -> [Int] -> [Int]
multN' n list = map ((\x y -> x*y)n) list

--Aplica em uma lista a função 3*x+2
applyExpr :: [Int] -> [Int]
applyExpr list = [3*x+2 | x <-list]

--Aplica em uma lista a função 3*x+2 com lambda
applyExpr' :: [Int] -> [Int]
applyExpr' list = map (\x -> 3*x+2) list

--Adiciona um sufixo a uma lista
addSuffix :: String -> [String] -> [String]
addSuffix suf list = [x ++ suf | x <- list]

--Recebe uma lista e retorna uma lista somente com números maiores que 5
selectgt5 :: [Int] -> [Int]
selectgt5 list = [x | x <- list, x > 5]

--Soma os valores ímpares de uma lista recebida e retorna o somatório
sumOdds :: [Int] -> Int
sumOdds list = sum [x | x <- list, odd x]

--Soma os valores ímpares de uma lista recebida e retorna o somatório
sumOdds' :: [Int] -> Int
sumOdds' list = sum (filter odd list)

--Selecionar somente valores pares entre 20 e 50
selectExpr :: [Int] -> [Int]
selectExpr list = [x | x <- list, even x && x >= 20 && x <= 50]

--Receba uma lista de palavras e retorne a quantidade de palavras dessa lista que possuem menos de 5 caracteres
countShorts :: [String] -> Int
countShorts list = length [x | x <- list, length x < 5]

--Calcula x^2/2 para cada elemento x da lista e retorna apenas os que forem maiores que 10
calcExpr :: [Float] -> [Float]
calcExpr list = [(x^2)/2 | x <- list, (x^2)/2 > 10]

--Converte espaçoes em traços em uma string
trSpaces :: String -> String
trSpaces str = [if x == ' ' then '-' else x | x <- str]
--filter (/= ' ') str

--Receba uma lista de tuplas e retorne apenas o segundo elemento
selectSnd :: [(Int,Int)] -> [Int]
selectSnd list = [snd x | x <- list]

--Denovo, acho que o jeito esperado era esse
selectSnd' :: [(Int,Int)] -> [Int]
selectSnd' list = map (\(_,y) -> y) [x | x <- list]

--Calcular o somatório dos pares de elementos da tupla criada
dotProd :: [Int] -> [Int] -> Int
dotProd list1 list2 = sum [fst x * snd x | x <- (zip list1 list2)]

--Testando do modo que eu acho que era o esperado
dotProd' :: [Int] -> [Int] -> Int
dotProd' list1 list2 = sum $ map (\(x,y) -> x * y) [x | x <- (zip list1 list2)]