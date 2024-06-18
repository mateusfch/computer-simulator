-- tipos simbolicos
type Address = Int
type Value = Int

-- lista de tuplas (endereço, valor) representa a memoria
type Memory = [(Address, Value)] -- 0 a 255 (teoricamente)

-- estrutura de dados que representa o estado da CPU
data CPU = CPU {
    acc :: Value,   -- Acumulador
    pc :: Address,  -- Program Counter
    eqz :: Bool     -- Flag EQZ
} deriving (Show)

-- instruções possíveis
data Instruction = LOD Address
                 | STO Address
                 | JMP Address
                 | JMZ Address
                 | CPE Address
                 | ADD Address
                 | SUB Address
                 | NOP
                 | HLT
                 deriving (Show, Eq)


-- funcao para encontrar o valor que esta no endereco de memoria especificado
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
fetchValue :: Address -> Memory -> Value
fetchValue addr mem =
    case lookup addr mem of
        Just val -> val
        Nothing -> error "Endereco de memoria nao encontrado"

-- lod: Carrega o conteúdo do endereço de memória <end> no registrador acumulador (ACC).
lod :: Address -> Memory -> CPU -> CPU
lod addr mem cpu = cpu {acc = fetchValue addr mem}

-- sto: Armazena o conteúdo do registrador acumulador (ACC) no endereço de memória <end>.
sto :: Address -> Memory -> CPU -> Memory
sto addr [] cpu = []
sto addr ((addr_t,val_t):xs) cpu 
    | addr_t == addr = (addr_t, acc cpu):xs 
    | otherwise = (addr_t, val_t) : sto addr xs cpu 



-- jmp: Desvio incondicional: carrega no contador de instruções o valor <end> forçando com que a próxima instrução
-- a ser executada seja a que se encontra no endereço de memória <end>
jmp :: Address -> CPU -> CPU 
jmp addr cpu = cpu{pc = addr}

-- jmz: Desvio condicional: funcionamento análogo ao da instrução JMP com a diferença que a carga do contador de instruções 
-- só ocorre se o valor do acumulador for igual a zero (de acordo com a flag EQZ).
jmz :: Address -> CPU -> CPU 
jmz addr cpu = if eqz cpu then cpu{pc = addr} else cpu{pc = pc cpu + 2}

-- add: Adiciona o conteúdo do endereço de memória <end> ao conteúdo armazenado no acumulador (ACC) e armazena a resposta no próprio acumulador.
add :: Address -> Memory -> CPU -> CPU
add addr mem cpu = cpu {acc = fetchValue addr mem + acc cpu }

-- sub: Subtrai o conteúdo do endereço de memória <end> do conteúdo do acumulador (ACC) e armazena a resposta no próprio acumulador.
sub :: Address -> Memory -> CPU -> CPU
sub addr mem cpu = cpu {acc = fetchValue addr mem - acc cpu }

-- cpe: Se o conteúdo do endereço <end> for igual ao acumulador, coloca 0 no acumulador, caso contrário coloca 1
cpe :: Address -> Memory -> CPU -> CPU
cpe addr mem cpu = if acc cpu == fetchValue addr mem then cpu{acc=0, eqz=True} else cpu{acc=1, eqz=False}


-- nop: Não executa ação nenhuma (No OPeration).
nop :: CPU -> CPU 
nop cpu = cpu{pc = pc cpu + 2}

-- hlt: Encerra o ciclo de execução do processador (HaLT)