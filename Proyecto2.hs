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
-- Definamos Eq para Alteracion
-- Defini Eq para alteracion también porque sino me decía que no podia comprara NotaMusical porque Alteración no tenia la Clase Eq

instance Eq Alteracion
    where
        Bemol == Bemol = True
        Natural == Natural = True
        Sostenido == Sostenido = True
        _ == _ = False

-- Prueba
--ghci> Bemol == Bemol
--True
--ghci> Natural == Natural
--True
--ghci> Sostenido == Sostenido
--True
--ghci> Bemol == Natural
--False
--ghci> Bemol == Sostenido
--False
--ghci> Natural == Sostenido
--False

-- Definamos Eq para NotaMusical

instance Eq NotaMusical
    where
        (Nota nota1 alteracion1) == (Nota nota2 alteracion2) = nota1 == nota2 && alteracion1 == alteracion2

-- Prueba
--ghci> let notaMusical1 = Nota Do Bemol
--ghci> let resultado1 = sonidoCromatico notaMusical1
--ghci> let notaMusical2 = Nota Re Natural
--ghci> let resultado2 = sonidoCromatico notaMusical2
--ghci> let notaMusical3 = Nota Mi Sostenido
--ghci> let resultado3 = sonidoCromatico notaMusical3
--ghci> let notaMusical4 = Nota Fa Bemol
--ghci> let resultado4 = sonidoCromatico notaMusical4
--ghci> let notaMusical5 = Nota Sol Natural
--ghci> let resultado5 = sonidoCromatico notaMusical5
--ghci> let notaMusical6 = Nota La Sostenido
--ghci> let resultado6 = sonidoCromatico notaMusical6
--ghci> let notaMusical7 = Nota Si Natural
--ghci> let resultado7 = sonidoCromatico notaMusical7
--ghci> resultado1 == resultado2
--False
--ghci> resultado1 == resultado3
--False
--ghci> resultado2 == resultado4
--False
--ghci> resultado3 == resultado5
--False
--ghci> resultado5 == resultado6
--False
--ghci> resultado1 == resultado1
--True
--ghci> resultado2 == resultado2
--True
--ghci> resultado3 == resultado3
--True
--ghci> resultado4 == resultado4
--True
--ghci> resultado5 == resultado5
--True
--ghci> resultado6 == resultado6
--True
--ghci> resultado7 == resultado7
--True

--f
-- Definamos Ord para NotaMusical
-- Preguntar en clase si está bien

instance Ord NotaMusical where
    (Nota nota1 alteracion1) <= (Nota nota2 alteracion2) =
        case compare (sonidoCromatico (Nota nota1 alteracion1)) (sonidoCromatico(Nota nota2 alteracion2)) of
            LT -> True -- LT devuelve True porque LT considera (sonidoCromatico (Nota nota1 alteracion1)) es menor que (sonidoCromatico(Nota nota2 alteracion2))
            EQ -> True -- EQ devuelve True porque EQ considera (sonidoCromatico (Nota nota1 alteracion1)) es igual que (sonidoCromatico(Nota nota2 alteracion2))
            GT -> False -- GT devuelve False porque GT considera (sonidoCromatico (Nota nota1 alteracion1)) es mayor que (sonidoCromatico(Nota nota2 alteracion2))
-- compare :: Ord a => a -> a -> Ordering

-- Prueba
--ghci> let notaMusical1 = Nota Do Bemol
--ghci> let resultado1 = sonidoCromatico notaMusical1
--ghci> let notaMusical2 = Nota Re Natural
--ghci> let resultado2 = sonidoCromatico notaMusical2
--ghci> let notaMusical3 = Nota Mi Sostenido
--ghci> let resultado3 = sonidoCromatico notaMusical3
--ghci> let notaMusical4 = Nota Fa Bemol
--ghci> let resultado4 = sonidoCromatico notaMusical4
--ghci> let notaMusical5 = Nota Sol Natural
--ghci> let resultado5 = sonidoCromatico notaMusical5
--ghci> let notaMusical6 = Nota La Sostenido
--ghci> let resultado6 = sonidoCromatico notaMusical6
--ghci> let notaMusical7 = Nota Si Natural
--ghci> let resultado7 = sonidoCromatico notaMusical7
--ghci> resultado1 <= resultado2
--True
--ghci> resultado1 <= resultado3
--True
--ghci> resultado1 <= resultado1
--True
--ghci> resultado5 <= resultado1
--False
--ghci> resultado5 <= resultado2
--False
--ghci> resultado7 <= resultado6
--False

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

-- Definimos Show para Deportista Sino no me meuestra nada la funcion atender

instance Show Zona where
    show Arco = "Arco"
    show Defensa = "Defensa"
    show Mediocampo = "Mediocampo"
    show Delantera = "Delantera"

instance Show TipoReves where
    show DosManos = "DosManos"
    show UnaMano = "UnaMano"

instance Show Modalidad where
    show Carretera = "Carretera"
    show Pista = "Pista"
    show Monte = "Monte"
    show BMX = "BMX"

instance Show PiernaHabil where
    show Izquierda = "Izquierda"
    show Derecha = "Derecha"

instance Show Deportista where
    show Ajedrecista = "Ajedrecista"
    show (Ciclista modalidad) = "Ciclista " ++ show modalidad
    show (Velocista altura) = "Velocista " ++ show altura
    show (Tenista tipoReves manoHabil altura) =
        "Tenista " ++ show tipoReves ++ " " ++ show manoHabil ++ " " ++ show altura
    show (Futbolista zona numCamiseta piernaHabil altura) =
        "Futbolista " ++ show zona ++ " " ++ show numCamiseta ++ " " ++ show piernaHabil ++ " " ++ show altura
--

-- Ejercicio 7 Tipos Recursivos
-- Definición del tipo Cola

data Cola = VaciaC | Encolada Deportista Cola

instance Show Cola where
    show VaciaC = "VaciaC"
    show (Encolada deportista cola) = show deportista ++ ", " ++ show cola

-- Inciso a
-- 1
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada _ cola) = Just cola

-- Prueba

--ghci> let cola1 = Encolada Ajedrecista VaciaC
--ghci> let resultado1 = atender cola1
--ghci> print resultado1
--Just VaciaC

--ghci> let cola2 = Encolada (Futbolista Arco 1 Derecha 175) (Encolada Ajedrecista VaciaC)
--ghci> let resultado2 = atender cola2
--ghci> print resultado2
--Just Ajedrecista, VaciaC

--ghci> let cola3 = Encolada (Ciclista BMX) (Encolada Ajedrecista (Encolada (Ciclista Monte) VaciaC))
--ghci> let resultado3 = atender cola3
--ghci> print resultado3
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
