-- tipos 
type Address = Int
type Value = Int

-- lista de tuplas (endereço, valor) representa a memoria
type Memory = [(Address, Value)] -- 0 a 255 (teoricamente)

-- estrutura de dados que representa o estado da CPU
data CPU = CPU {
    acc :: Value,   
    pc :: Address,  
    eqz :: Bool,     
    instructionRegister :: (Int, Int) 
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
    | otherwise = (addr_t, val_t):sto addr xs cpu 

-- instructionExe: executa a instrucao corrente e atualiza a memoria e a cpu quando necessario
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

-- loadInstruction: carrega o registrador de instrucoes e o pc
loadInstruction :: Memory -> CPU -> CPU
loadInstruction mem cpu = 
    let 
    instr = (fetchValue(pc cpu) mem, fetchValue(pc cpu +1) mem)
    in cpu {instructionRegister = instr, pc = pc cpu + 2}

simulateComputer :: Memory -> CPU -> (Memory, CPU) 
simulateComputer mem cpu = 
   -- primeiramente, eh preciso carregar o registrador de instrucoes com a instrucao a ser executada e atualizar o pc
    let cpu1 = loadInstruction mem cpu 
        -- depois, executo a instrucao e recebo a memoria e a cpu atualizadas
        (updatedMem, updatedCPU) = instructionExe (instructionRegister cpu1) mem cpu1
        in if fst (instructionRegister cpu1) == 20 then (updatedMem, updatedCPU) else simulateComputer updatedMem updatedCPU



-- Função para testar o programa
main :: IO ()
main = do
    
    -- Exemplo (1) 
    -- Resp = A + B – 2
    
    -- let mem = [
    --             (0,2),(1,240), -- lod: carrega o valor de A no acc
    --             (2,14),(3,241), -- add: soma ao acc o valor de B
    --             (4,16), (5,245), -- sub: subtrai do acc a cte 2
    --             (6,4),(7,251), -- sto: carrega em resp o valor do acc
    --             (8,20),(9,18), -- hlt: encerra o ciclo de execucao do processador
    --             (240,100), -- A
    --             (241,50), -- B
    --             (245,2), -- valor cte 2
    --             (251,0), -- resp
    --             (252,0), -- endereco de video
    --             (253,0), -- endereco de video
    --             (254,0), -- endereco de video
    --             (255,0) -- endereco de video
    --             ] -- endereco de video
    
    -- Exemplo (2)
    -- Resp = A * B;

    -- let mem = [
    --             (0, 2), (1, 240), -- lod: armazenei o valor de A no acumulador
    --             (2, 8), (3,24), -- jmz: 0 * B --> resp=0,
    --             (4,2), (5,241), -- lod: armazenei o valor de B no acumulador
    --             (6,8), (7,24), -- jmz: A * 0 --> resp = 0

    --             (8,16), (9,246), -- sub: subtrai uma unidade do acumulador 
    --             (10, 4), (11, 242), -- sto: carrego no end. 242 o estado do acumulador subtraido
    --             (12, 2), (13, 251), -- lod: carrego o valor atual de resp no acumulador
    --             (14,14), (15,240),   -- add: somo A no acumulador
    --             (16, 4), (17, 251), -- sto: carrego o resultado da soma em resp
                
    --             (18, 2), (19, 242), -- lod: carrego no acumulador o valor de B sendo subtraido
    --             (20, 8), (21,24), -- jmz: desvio (jmz) pro fim se acumulador = 0
    --             (22, 6), (23,8), -- jmp: desvio (jmp) pro inicio pra uma nova iteracao 
    --             (24, 20), (25,18), -- hlt: encerra o ciclo de execucao do processador
                
    --             (240,2), -- A
    --             (241,2), -- B
    --             (242,0), -- B sendo subtraido
    --             (246, 1), -- cte 1
    --             (251,0), -- resp
    --             (252,0), -- endereco de video
    --             (253,0), -- endereco de video
    --             (254,0), -- endereco de video
    --             (255,0) -- endereco de video
    --         ]
   
    
    -- Exemplo (3)
    -- A = 0; Resp = 1; while(A < 5) { A = A + 1; Resp = Resp + 2; }
    let mem = [
                  (0, 2), (1,240), -- lod: carrega o valor de A no acc
                  (2, 10), (3,241), -- cpe: A == 5 ? acc=0:1
                  (4, 8), (5, 20), -- jmz: acc==0 --> fim
                  (6, 2), (7,240), -- lod: carrega o valor de A no acc
                  (8, 14), (9, 245), -- jmz: soma +1 ao acc
                  (10,4), (11,240), -- sto: carrega o valor de acc em A
                  
                  (12,2), (13, 251), -- lod: carrega o valor de resp no acc
                  (14,14), (15,246), -- add: soma +2 ao acc
                  (16, 4), (17, 251), -- sto: carrega o valor de acc em resp

                  (18,6), (19,0), -- jmp: volta para 0, para continuar a iteracao
                  (20, 20), (21,18), --hlt: -- encerra o ciclo de execucao

                  (240, 0), -- A
                  (241, 5), -- valor associado ao  loop
                  (245, 1), -- valor cte que será somado ao A (A = A + 1)
                  (246, 2), -- valor cte que será somado ao resp
                  (251, 1), -- resp
                  (252,0), -- endereco de video
                  (253,0), -- endereco de video
                  (254,0), -- endereco de video
                  (255,0) -- endereco de video
            ]
    let cpu = CPU {acc = 0, pc = 0, eqz = False, instructionRegister = (0,0)}
    let (finalMem, finalCPU) = simulateComputer mem cpu
    print finalMem
    putStrLn "TELA - Enderecos de memoria de video"
    putStrLn $ "ENDERECO 251: " ++ show (fetchValue 251 finalMem)
    putStrLn $ "ENDERECO 252: " ++ show (fetchValue 252 finalMem)
    putStrLn $ "ENDERECO 253: " ++ show (fetchValue 253 finalMem)
    putStrLn $ "ENDERECO 254: " ++ show (fetchValue 254 finalMem)
    putStrLn $ "ENDERECO 255: " ++ show (fetchValue 255 finalMem)


