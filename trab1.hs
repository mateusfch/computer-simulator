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

-- // [  [1, 200]     ]