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



-- fetchValue: encontra o valor que esta no endereco de memoria especificado
fetchValue :: Address -> Memory -> Value
fetchValue addr mem =
    case lookup addr mem of
        Just val -> val 
        Nothing -> error ("Endereco de memoria nao encontrado: " ++ show addr)


-- sto: armazena o conteúdo do registrador acumulador (ACC) no endereço de memória <end>.
sto :: Address -> Memory -> CPU -> Memory
sto addr [] cpu = []
sto addr ((addr_t,val_t):xs) cpu 
    | addr_t == addr = (addr_t, acc cpu):xs 
    | otherwise = (addr_t, val_t) : sto addr xs cpu 

instructionExe :: (Int, Int) -> Memory -> CPU -> (Memory, CPU)
instructionExe (opcode, addr) mem cpu = 
    case opcode of 
        2 -> (mem, updateAcc (fetchValue addr mem) cpu)
        4  -> (sto addr mem cpu, cpu)
        6  -> (mem, cpu {pc = addr})
        8  -> if eqz cpu then (mem, cpu {pc = addr}) else (mem, cpu {pc = pc cpu})
        10 -> if acc cpu == fetchValue addr mem then (mem, cpu {acc = 0, eqz = True}) else (mem, cpu {acc = 1, eqz = False})        
        14 -> (mem, updateAcc (acc cpu + fetchValue addr mem) cpu)
        16 -> (mem, updateAcc (acc cpu - fetchValue addr mem) cpu)
        18 -> (mem, cpu)
        20 -> (mem, cpu)  

-- updateAcc: atualiza o acumulador e a flag eqz
updateAcc :: Value -> CPU -> CPU
updateAcc newAcc cpu = cpu { acc = newAcc, eqz = newAcc == 0 }

-- execution: carrega o registrador de instruções e atualiza o contador de instruções
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
    -- let mem = [(0,2),(1,240),
    --             (2,14),(3,241),
    --             (4,16), (5,242),
    --             (6,4),(7,251),
    --             (8,20),(9,18),
    --             (240,100),
    --             (241,50),
    --             (242,2),
    --             (251,0)]
    
    -- Exemplo (2)
    -- let mem = [
    --             -- op1:3  op2:4 --> res = op1*op2
    --             (0,2),(1, 240), -- armazenei 3 no acumulador
    --             -- (2,4),(3,242), -- carreguei o valor do acumulador (3) no end. 242
    --             (2,2),(3,241), -- armazenei 4 no acumulador 
    --             (4,4),(5,243),  -- carreguei o valor do acumulador (4) no end. 243
                
    --             (6,16), (7,246), -- (1a passada) acumulador = 4 --> subtrai 1 do 4 --> acumulador = 3
    --             (8, 4), (9, 243), -- carreguei no end. 243 o estado do acumulador subtraido
    --             (10, 2), (11, 242), -- carreguei no acumulador o valor de 242(inicialmente 0)
    --             (12,14), (13,240),   -- soma com 3 (conteudo do 240)
    --             (14, 4), (15, 242), -- carreguei o valor da soma no 242 
    --             (16, 2), (17, 243),
    --             (18, 8), (19,22),
    --             (20, 6), (21,6),
    --             (22, 20), (23,18),
    --             (240,13),
    --             (241,4),
    --             (242,0),
    --             (243,0),
    --             (244,0),
    --             (245,0),
    --             (246,1)
    --         ]
    -- Exemplo (3)
    let mem = [
                  (0, 2), (1,240),
                  (2, 14), (3, 245),
                  (4,4), (5,240),
                  
                  (6,2), (7, 251),
                  (8,14), (9,246),
                  (10, 4), (11, 251),

                  (12, 2), (13, 241),
                  (14, 16), (15, 245),
                  (16, 4), (17, 241),

                  (18,8), (19,22),
                  (20, 6), (21, 0),
                  (22, 20), (23, 18),
                  
                  (240, 0), -- a
                  (241, 5), -- contador do loop
                  (245, 1), -- valor cte que será somado ao a (a = a + 1)
                  (246, 2), -- valor cte que será somado ao resp
                  (251, 1) -- resp

            ]
    let cpu = CPU {acc = 0, pc = 0, eqz = False, instructionRegister = (0,0)}
    let (finalMem, finalCPU) = simulateComputer mem cpu
    print finalCPU
    print finalMem

-- 