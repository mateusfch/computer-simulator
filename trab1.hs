-- tipos simbolicos
type Address = Int
type Value = Int

-- lista de tuplas (endereço, valor) representa a memoria
type Memory = [(Address, Value)] -- 0 a 255 (teoricamente)

-- estrutura de dados que representa o estado da CPU
data CPU = CPU {
    acc :: Value,   -- Acumulador
    pc :: Address,  -- Program Counter
    eqz :: Bool,     -- Flag EQZ
    instructionRegister :: (Int, Int) -- Registrador de instrucoes
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



-- sto: Armazena o conteúdo do registrador acumulador (ACC) no endereço de memória <end>.
sto :: Address -> Memory -> CPU -> Memory
sto addr [] cpu = []
sto addr ((addr_t,val_t):xs) cpu 
    | addr_t == addr = (addr_t, acc cpu):xs 
    | otherwise = (addr_t, val_t) : sto addr xs cpu 



instructionExe :: (Int, Int) -> Memory -> CPU -> (Memory, CPU)
instructionExe (opcode, addr) mem cpu = 
    case opcode of 
        2 -> (mem, cpu {acc = fetchValue addr mem})
        4  -> (sto addr mem cpu, cpu)
        6  -> (mem, cpu {pc = addr})
        8  -> if eqz cpu then (mem, cpu {pc = addr}) else (mem, cpu {pc = pc cpu + 2})
        10 -> if acc cpu == fetchValue addr mem then (mem, cpu {acc = 0, eqz = True}) else (mem, cpu {acc = 1, eqz = False})        
        14 -> (mem, cpu {acc = acc cpu + fetchValue addr mem})
        16 -> (mem, cpu {acc = acc cpu - fetchValue addr mem})
        18 -> (mem, cpu {acc = acc cpu * fetchValue addr mem})
        20 -> (mem, cpu)  


-- execution: para carregar o registrador de instruções e atualizar o contador de instruções
execution :: Memory -> CPU -> CPU
execution mem cpu = 
    let 
    instr = (fetchValue(pc cpu) mem, fetchValue(pc cpu +1) mem)
    in cpu {instructionRegister = instr, pc = pc cpu + 2}


simulateComputer :: Memory -> CPU -> (Memory, CPU) 
simulateComputer mem cpu = 
   -- primeiramente, eh preciso carregar o registrador de instrucoes com a instrucao a ser executada
    let cpu1 = execution mem cpu 
        -- depois, executo a instrucao e recebo a memoria e a cpu atualizadas
        (updatedMem, updatedCPU) = instructionExe (instructionRegister cpu1) mem cpu1
        in if fst (instructionRegister cpu1) == 20 then (updatedMem, updatedCPU) else simulateComputer updatedMem updatedCPU


-- Função para testar o programa
main :: IO ()
main = do
    -- Exemplo (1) 
    -- let mem = [
    --             (0, 2), (1, 240),  -- LOD 240 // carrega o valor do endereço 240 no acc
    --             (2, 14), (3, 241), -- 2 ADD 241 // soma o valor do acc(end 240) com o valor do 241
    --             (4, 4), (5, 251),  -- 4 STO 251 // carrega em 251 o resultado do acc
    --             (6, 20)            -- HLT // pra encerrar
    --            ]
    let mem = [(0,2),(1,240),
                (2,14),(3,241),
                (4,16), (5,242),
                (6,4),(7,251),
                (8,20),(9,18),
                (240,100),
                (241,50),
                (242,2),
                (251,0)]
        cpu = CPU {acc = 0, pc = 0, eqz = False, instructionRegister = (0,0)}
        (finalMem, finalCPU) = simulateComputer mem cpu
    print finalCPU
    print finalMem
















-- DEFINICAO DAS INSTRUCOES COMO FUNCOES

-- -- lod: Carrega o conteúdo do endereço de memória <end> no registrador acumulador (ACC).
-- lod :: Address -> Memory -> CPU -> CPU
-- lod addr mem cpu = cpu {acc = fetchValue addr mem}




-- jmp: Desvio incondicional: carrega no contador de instruções o valor <end> forçando com que a próxima instrução
-- a ser executada seja a que se encontra no endereço de memória <end>
-- jmp :: Address -> CPU -> CPU 
-- jmp addr cpu = cpu{pc = addr}

-- -- jmz: Desvio condicional: funcionamento análogo ao da instrução JMP com a diferença que a carga do contador de instruções 
-- -- só ocorre se o valor do acumulador for igual a zero (de acordo com a flag EQZ).
-- jmz :: Address -> CPU -> CPU 
-- jmz addr cpu = if eqz cpu then cpu{pc = addr} else cpu{pc = pc cpu + 2}

-- -- add: Adiciona o conteúdo do endereço de memória <end> ao conteúdo armazenado no acumulador (ACC) e armazena a resposta no próprio acumulador.
-- add :: Address -> Memory -> CPU -> CPU
-- add addr mem cpu = cpu {acc = fetchValue addr mem + acc cpu }

-- -- sub: Subtrai o conteúdo do endereço de memória <end> do conteúdo do acumulador (ACC) e armazena a resposta no próprio acumulador.
-- sub :: Address -> Memory -> CPU -> CPU
-- sub addr mem cpu = cpu {acc = fetchValue addr mem - acc cpu }

-- -- cpe: Se o conteúdo do endereço <end> for igual ao acumulador, coloca 0 no acumulador, caso contrário coloca 1
-- cpe :: Address -> Memory -> CPU -> CPU
-- cpe addr mem cpu = if acc cpu == fetchValue addr mem then cpu{acc=0, eqz=True} else cpu{acc=1, eqz=False}

-- -- nop: Não executa ação nenhuma (No OPeration).
-- nop :: CPU -> CPU 
-- nop cpu = cpu{pc = pc cpu + 2}