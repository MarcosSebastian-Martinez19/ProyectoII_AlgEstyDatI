-- Ejercicio 1
--a
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving Eq

--b
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Física"
titulo Computacion = "Licenciatura en Ciencias de la Computación"
titulo Astronomia = "Licenciatura en Astronomía"

-- Prueba
--ghci> titulo Matematica
--"Licenciatura en Matemática"
--ghci> titulo Fisica
--"Licenciatura en Física"
--ghci> titulo Computacion
--"Licenciatura en Ciencias de la Computación"
--ghci> titulo Astronomia
--"Licenciatura en Astronomía"

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

-- Prueba
--ghci> :t Ciclista
--Ciclista :: Modalidad -> Deportista

--c
contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas (x:xs) =
    case x of
        Velocista _ -> 1 + contar_velocistas xs
        _ -> contar_velocistas xs

-- Prueba
--ghci> contar_velocistas [Velocista 180, Velocista 10]
--2
--ghci> contar_velocistas [Velocista 180, Velocista 10, Ciclista BMX]
--2
--ghci> contar_velocistas [Velocista 180, Ciclista BMX]
--1

--d
contar_futbolista :: [Deportista] -> zona -> Int
contar_futbolista [] z = 0
contar_futbolista (_:xs) z = contar_futbolista xs z
contar_futbolista (Futbolista Arco _ _ _ ) Arco = 1 + contar_futbolista xs Arco
contar_futbolista (Futbolista Defensa _ _ _ ) Defensa = 1 + contar_futbolista xs Defensa
contar_futbolista (Futbolista Mediocampo _ _ _ ) Mediocampo = 1 + contar_futbolista xs Mediocampo
contar_futbolista (Futbolista Delantera _ _ _ ) Delantera = 1 + contar_futbolista xs Delantera
--Futbolista Zona NumCamiseta PiernaHabil Altura
--data Zona = Arco | Defensa | Mediocampo | Delantera