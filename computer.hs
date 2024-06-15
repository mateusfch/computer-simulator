-- tipos simbolicos
type Address = Int
type Value = Int

-- lista de tuplas (endereço, valor) representa a memoria
type Memory = [(Address, Value)] -- 0 a 255 (teoricamente)



-- funcao para encontrar o valor que esta no endereco de memoria especificado
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
fetchValue :: Address -> Memory -> Value
fetchValue addr mem =
    case lookup addr mem of
        Just val -> val
        Nothing -> error "Endereco de memoria nao encontrado"



--value | (address, value) <- mem, address == addr











-- vazia :: [a] -> Bool
-- vazia [] = True
-- vazia (_:_) = False

-- procura :: Int -> [Int] -> Bool 
-- procura _ [] = False 
-- procura a (x:xs) = if a==x then True else procura a xs

