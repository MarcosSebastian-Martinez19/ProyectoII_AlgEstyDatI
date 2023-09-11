-- Ejercicio 1 Tipos enumerados.

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

-- Ejercicio 3 Polimorfismo ad hoc.

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

-- Ejercicio 4 Sinonimos de tipos; constructores con parámetros.

-- Sinónimos de tipo
type Altura = Int
type NumCamiseta = Int

-- Tipos algebráicos sin parámetros (aka enumerados)
data Zona = Arco | Defensa | Mediocampo | Delantera deriving Show
data TipoReves = DosManos | UnaMano deriving Show
data Modalidad = Carretera | Pista | Monte | BMX deriving Show
data PiernaHabil = Izquierda | Derecha deriving Show

-- Sinónimo
type ManoHabil = PiernaHabil

-- Deportista es un tipo algebráico con constructores paramétricos
data Deportista = Ajedrecista                           -- Constructor sin argumentos
    | Ciclista Modalidad                                -- Constructor con un argumento
    | Velocista Altura                                  -- Constructor con un argumento
    | Tenista TipoReves ManoHabil Altura                -- Constructor con tres argumentos
    | Futbolista Zona NumCamiseta PiernaHabil Altura    -- Constructor con 4 argumentos
    deriving Show
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
-- 2 formas de hacerlo
-- La primera
contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] z = 0
contar_futbolistas (Futbolista zona _ _ _:xs) z
    | verificarZona z zona = 1 + contar_futbolistas xs z
    | otherwise = contar_futbolistas xs z
contar_futbolistas (_:xs) z = contar_futbolistas xs z

verificarZona :: Zona -> Zona -> Bool
verificarZona Arco Arco = True
verificarZona Defensa Defensa = True
verificarZona Mediocampo Mediocampo = True
verificarZona Delantera Delantera = True
verificarZona _ _ = False

-- Prueba
--ghci> contar_futbolistas [Futbolista Arco 10 Izquierda 180, Futbolista Arco 5 Derecha 170, Ciclista Pista, Ajedrecista, Futbolista Defensa 12 Izquierda 190] Arco
--2
--ghci> contar_futbolistas [Futbolista Arco 10 Izquierda 180, Futbolista Arco 5 Derecha 170, Ciclista Pista, Ajedrecista, Futbolista Defensa 12 Izquierda 190, Futbolista Mediocampo 10 Derecha 178] Mediocampo
--1
--ghci> contar_futbolistas [] Arco
--0
--ghci> contar_futbolistas [] Defensa 
--0
--ghci> contar_futbolistas [] Mediocampo
--0
--ghci> contar_futbolistas [] Delantera
--0

-- La segunda
--contar_futbolistas' :: [Deportista] -> Zona -> Int
--contar_futbolistas' [] z = 0
--contar_futbolistas' (Futbolista zona _ _ _ :xs) z = case verificarZona z zona of
--    True -> 1 + contar_futbolistas xs z
--    False -> contar_futbolistas xs z
-- Prueba
--ghci> contar_futbolistas' [Futbolista Arco 10 Izquierda 180, Futbolista Arco 5 Derecha 170, Ciclista Pista, Ajedrecista, Futbolista Defensa 12 Izquierda 190] Arco
--2
--ghci> contar_futbolistas' [Futbolista Arco 10 Izquierda 180, Futbolista Arco 5 Derecha 170, Ciclista Pista, Ajedrecista, Futbolista Defensa 12 Izquierda 190, Futbolista Mediocampo 10 Derecha 178] Mediocampo
--1
--ghci> contar_futbolistas' [] Arco
--0
--ghci> contar_futbolistas' [] Defensa
--0
--ghci> contar_futbolistas' [] Mediocampo
--0
--ghci> contar_futbolistas' [] Delantera
--0

-- e
-- Preguntar en clase
contar_futbolistasFilter :: [Deportista] -> Zona -> Int
contar_futbolistasFilter deportistas zona = length (filter (verificarZonaFutbolista zona ) deportistas)

verificarZonaFutbolista :: Zona -> Deportista -> Bool
verificarZonaFutbolista z (Futbolista zona _ _ _) = verificarZona z zona
verificarZonaFutbolista _ _ = False

-- Prueba
-- ghci> contar_futbolistasFilter  [Futbolista Arco 10 Izquierda 180, Futbolista Arco 5 Derecha 170, Ciclista Pista, Ajedrecista, Futbolista Defensa 12 Izquierda 190] Arco
-- 2
-- ghci> contar_futbolistasFilter [Futbolista Arco 10 Izquierda 180, Futbolista Arco 5 Derecha 170, Ciclista Pista, Ajedrecista, Futbolista Defensa 12 Izquierda 190, Futbolista Mediocampo 10 Derecha 178] Mediocampo
-- 1
-- ghci> contar_futbolistasFilter  [] Arco
-- 0
-- ghci> contar_futbolistasFilter  [] Defensa
-- 0
-- ghci> contar_futbolistasFilter  [] Mediocampo
-- 0
-- ghci> contar_futbolistasFilter  [] Delantera
-- 0

-- Ejercicio 5 Definición de Clases.

-- a
sonidoNatural :: NotaBasica -> Int
sonidoNatural Do = 0
sonidoNatural Re = 2
sonidoNatural Mi = 4
sonidoNatural Fa = 5
sonidoNatural Sol = 7
sonidoNatural La = 9
sonidoNatural Si = 11

-- Prueba
--ghci> sonidoNatural Do
--0
--ghci> sonidoNatural Re
--2
--ghci> sonidoNatural Mi
--4
--ghci> sonidoNatural Fa
--5
--ghci> sonidoNatural Sol
--7
--ghci> sonidoNatural La
--9
--ghci> sonidoNatural Si
--11

-- b
data Alteracion = Bemol | Natural | Sostenido

-- c
data NotaMusical = Nota NotaBasica Alteracion

-- d

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota nota alteracion) = 
    case alteracion of
        Bemol -> sonidoNatural nota - 1
        Natural -> sonidoNatural nota
        Sostenido -> sonidoNatural nota +1

-- Prueba

--ghci> let notaMusical1 = Nota Do Bemol
--ghci> let resultado1 = sonidoCromatico notaMusical1
--ghci> print resultado1
---1
--ghci> let notaMusical2 = Nota Re Natural
--ghci> let resultado2 = sonidoCromatico notaMusical2
--ghci> print resultado2
--2
--ghci> let notaMusical3 = Nota Mi Sostenido
--ghci> let resultado3 = sonidoCromatico notaMusical3
--ghci> print resultado3
--5
--ghci> let notaMusical4 = Nota Fa Bemol
--ghci> let resultado4 = sonidoCromatico notaMusical4
--ghci> print resultado4
--4
--ghci> let notaMusical5 = Nota Sol Natural
--ghci> let resultado5 = sonidoCromatico notaMusical5
--ghci> print resultado5
--7
--ghci> let notaMusical6 = Nota La Sostenido
--ghci> let resultado6 = sonidoCromatico notaMusical6
--ghci> print resultado6
--10
--ghci> let notaMusical7 = Nota Si Natural
--ghci> let resultado7 = sonidoCromatico notaMusical7
--ghci> print resultado7
--11

-- e
-- Incluí el tipo NotaMusical a la clase Eq de manera tal que dos notas que tengan el mismo valor de sonidoCromatico se consideren iguales.

-- Definamos Eq para NotaMusical

instance Eq NotaMusical
    where
        (Nota nota1 alteracion1) == (Nota nota2 alteracion2) = sonidoCromatico(Nota nota1 alteracion1) == sonidoCromatico(Nota nota2 alteracion2)

-- Prueba
-- ghci> Nota Sol Natural  == Nota Sol Sostenido
-- False
-- ghci> Nota Mi Sostenido == Nota Mi Sostenido
-- True
-- ghci> Nota Do Bemol == Nota Do Bemol
-- True

--f
-- Definamos Ord para NotaMusical

instance Ord NotaMusical where
    (Nota nota1 alteracion1) <= (Nota nota2 alteracion2) = sonidoCromatico (Nota nota1 alteracion1) <= sonidoCromatico(Nota nota2 alteracion2)

-- Prueba
-- ghci> Nota Do Bemol <= Nota Do Bemol
-- True
-- ghci> Nota Mi Sostenido <= Nota Mi Sostenido
-- True
-- ghci> Nota Mi Sostenido <= Nota Re Natural
-- False

-- Ejercicio 6 Tipos enumerados con polimorfismo.

--a

primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento xs = Just (xs !! 0)

-- Prueba
--ghci> primerElemento []
--Nothing
--ghci> primerElemento ["x"]
--Just "x"
--ghci> primerElemento ["x","a"]
--Just "x"

primerElemento' :: [a] -> Maybe a
primerElemento' [] = Nothing
primerElemento' xs = Just (head xs)

-- Prueba
--ghci> primerElemento' []
--Nothing
--ghci> primerElemento' ["x"]
--Just "x"
--ghci> primerElemento' ["x","a"]
--Just "x"

primerElemento'' :: [a] -> Maybe a
primerElemento'' [] = Nothing
primerElemento'' (x:xs) = Just (x)

-- Prueba
--ghci> primerElemento'' []
--Nothing
--ghci> primerElemento'' ["x"]
--Just "x"
--ghci> primerElemento'' ["x","a"]
--Just "x"


-- Ejercicio 7 Tipos Recursivos
-- Definición del tipo Cola

data Cola = VaciaC | Encolada Deportista Cola

instance Show Cola where
    show VaciaC = "VaciaC"
    show (Encolada deportista cola) = show deportista ++ ", " ++ show cola

-- Inciso adata Cola = VaciaC | Encolada Deportista Cola
-- 1
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada _ cola) = Just cola

-- Prueba
-- ghci> atender (Encolada Ajedrecista (Encolada (Futbolista Arco 1 Derecha 175) VaciaC))
-- Just Futbolista Arco 1 Derecha 175, VaciaC
-- ghci> atender (Encolada Ajedrecista VaciaC)
-- Just VaciaC
--ghci> atender (Encolada (Ciclista BMX) (Encolada Ajedrecista (Encolada (Ciclista Monte) VaciaC)))
--Just Ajedrecista, Ciclista Monte, VaciaC

-- 2
encolar :: Deportista -> Cola -> Cola
encolar d VaciaC = Encolada d VaciaC
encolar d (Encolada deportista cola) = Encolada deportista (encolar d cola)

-- Prueba

--ghci> encolar (Ciclista BMX) VaciaC
--Ciclista BMX, VaciaC
--ghci> encolar (Ciclista BMX) (Encolada Ajedrecista (Encolada (Futbolista Arco 1 Derecha 190) VaciaC))
--Ajedrecista, Futbolista Arco 1 Derecha 190, Ciclista BMX, VaciaC

-- 3
busca :: Cola -> Zona -> Maybe Deportista
busca VaciaC _ = Nothing
busca (Encolada deportista cola) z = case deportista of
    Futbolista z _ _ _  -> Just deportista
    _ -> busca cola z

-- Responder a la b del tipo de Cola
-- El tipo Cola se parece al Tipo Palabra de la filmina del teórico.
-- data Cola = VaciaC | Encolada Deportista Cola
-- data Palabra = PVacia | Agregar Char Palabra

-- Ejercicio 8 Tipos recursivos y polimórficos.
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b)

type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

-- A
-- El tipo ListaAsoc para representar la información almacenada en una guía telefónica se debe instanciar así:

type GuiaTelefonica = ListaAsoc Nombre Int
type Nombre = String

-- B
-- 1
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo x y la) = 1 + (la_long la)

-- Prueba

--ghci> la_long (Nodo "String" 10 (Nodo "String2" 20 Vacia))
--2
--ghci> la_long (Nodo "Marcos" False (Nodo "Sebas" True Vacia))
--2
--ghci> la_long (Nodo "Marcos" False (Nodo "Sebas" True (Nodo "Hola" False Vacia)))
--3