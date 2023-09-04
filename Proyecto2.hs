-- Ejercicio 1
--a
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving Eq

--b
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matemática"
titulo Fisica = "Licenciatura en Física"
titulo Computacion = "Licenciatura en Ciencias de la Computación"
titulo Astronomia = "Licenciatura en Astronomía"

--c
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si
    deriving (Eq, Ord, Bounded, Show)

--d
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

-- Ejercicio 3

--a
minimoElemento :: (Ord a) => [a] -> a
minimoElemento [n] = n
minimoElemento (x:xs) = min x (minimoElemento xs)
-- Prueba
--ghci> minimoElemento [1,2,3,4,(-6)]
---6
--ghci> minimoElemento ['a','b','z']
--'a'

--b
minimoElemento' :: (Bounded a, Ord a) => [a] -> a
minimoElemento' [] = minBound
minimoElemento' (x:xs) = min x (minimoElemento xs)

--c
-- Prueba
--ghci> minimoElemento [Fa, La, Sol, Re, Fa]
--Re

-- Ejercicio 4
-- Sinónimos de tipo
type Altura = Int
type NumCamiseta = Int

-- Tipos algebráicos sin parámetros (aka enumerados)
data Zona = Arco | Defensa | Mediocampo | Delantera
data TipoReves = DosManos | UnaMano
data Modalidad = Carretera | Pista | Monte | BMX
data PiernaHabil = Izquierda | Derecha

-- Sinónimo
type ManoHabil = PiernaHabil

-- Deportista es un tipo algebráico con constructores paramétricos
data Deportista = Ajedrecista                           -- Constructor sin argumentos
    | Ciclista Modalidad                                -- Constructor con un argumento
    | Velocista Altura                                  -- Constructor con un argumento
    | Tenista TipoReves ManoHabil Altura                -- Constructor con tres argumentos
    | Futbolista Zona NumCamiseta PiernaHabil Altura    -- Constructor con 4 argumentos

--b ¿Cuál es el tipo del constructor Ciclista?
-- El tipo del constructor Ciclista es toma una Modalidad y devuelve un tipo Deportista

--c
contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas (x:xs)= 1 + (contar_velocistas)