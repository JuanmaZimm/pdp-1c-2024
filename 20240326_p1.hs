curry' :: ((a,b) -> c) -> a -> b -> c
curry' f x y = f (x,y)
-- curry' f = \x -> \y -> f (x,y)

uncurry :: (a->b->c) -> (a,b)->c
uncurry f (x, y) = f x y

prod :: Int -> Int -> Int
prod x y = x * y


doble :: Int -> Int
doble x = prod 2 x  

doble2 :: Int -> Int
doble2 = prod 2

triple :: Int -> Int
triple = (*3)

esMayorDeEdad :: Int -> Bool
esMayorDeEdad = (>=18) 

comp :: (b -> c) -> (a->b) -> (a -> c)
comp f g = \x -> f (g x)
-- comp f g x = f (g x)

flip' :: (a->b->c) -> (b->a->c)
flip' f = \x y -> f y x

($$) :: (a->b) -> a -> b
($$) f x = f x

const' :: a -> b -> a
const' = \x y -> x

-- macros de listas

-- Extension: [1,2,3]

-- Secuencias: [2, 5..18] = [2,5,8,11,14,17]

-- Comprension: [(x,y) | x <- [0..5], y <- [0..3], x+y == 4] = [(0,4),(1,3),(2,2),(3,1),(4,0)]

-- Listas Infinitas:

--  naturales = [0..]
-- multiplosDe3 = [0,3..]
-- repeat "hola" = ["hola","hola,..]
-- primos = [x | x <- [2..], primo x]
-- infinitosUnos = 1:infinitosUnos

-- Evaluacion lazy
-- nUnos n = take n infinitosUnos

-- nUnos 5 = 1 : take (5-1) infinitosUnos
-- nUnos 5 = [1,1,1,1,1]
-- Si para algun termino, existe una reducc finita => la estrategia de reduccion lazy termina

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f [x] = x
mejorSegun f (x:xs)  | f x (mejorSegun f xs) = x
                     | otherwise = mejorSegun f xs

maximo :: Ord a => [a] -> a
maximo = mejorSegun (>=)

minimo :: Ord a => [a] -> a
minimo = mejorSegun (<=)

listaMasCorta :: [[a]] -> [a]
listaMasCorta = mejorSegun (\x y -> length x <= length y)

-- Esquemas de recursión sobre listas: filter

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) | p x = x : filter' p xs
                 | otherwise = filter' p xs

deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN n = filter' (\x -> length x == n)

soloPuntosFijosEnN :: Int -> [Int -> Int] -> [Int -> Int]
soloPuntosFijosEnN n = filter' (\f -> f n == n)

-- Esquemas de recursión sobre listas: map

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado = reverse.(map' reverse)

paresCuadrados :: [Int] -> [Int]
paresCuadrados = (map' (^2)).(filter even)

-- Desplegando la macro de las listas por comprension

listaComp :: (a -> b) -> (a -> Bool) -> [a] -> [b]
listaComp f p xs = map' f (filter' p xs)

-- Esquemas de recursion estructural sobre listas

-- foldr toma caso base el ultimo elemento de la lista
-- foldl toma caso base el primer elemento de la lista
-- la lista no debe ser vacia

mejorSegun1 :: (a -> a -> Bool) -> [a] -> a
mejorSegun1 f = foldr1 (\x y -> if f x y then x else y)

-- Recursion sobre listas
elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y r -> y == x || r) False

--sumaAlt :: [Int] -> Int
--sumaAlt [] = 0
--sumaAlt (x:[]) = x
--sumaAlt (x:y:xs) = x - y + sumaAlt xs
--sumaAlt (x:y:xs) = foldr (\x r -> x - y + r) 0 (y:xs)

--sacarPrimera :: Eq a => a -> [a] -> [a]
--sacarPrimera _ [] = []ghcup self remove

--sacarPrimera x (y:ys) | x == y = ys
--                      | otherwise = y : sacarPrimera x ys
--sacarPrimera :: Eq a => a -> [a] -> [a]
--sacarPrimera e = foldr 

-- folds sobre nuevas estructuras

data AEB a = Hoja a | Bin (AEB a) a (AEB a)

foldAEB :: (a->b) -> (b -> a -> b ->b) -> AEB a -> b
foldAEB fHoja fBin arbol = case arbol of
    Hoja n           -> fHoja n
    Bin t1 n t2      -> fBin (rec t1) n (rec t2)
                        where rec = foldAEB fHoja fBin

-- hacer las otras funcs
cantNodos :: AEB a -> Int
cantNodos = foldAEB (const 1) (\ri _ rd -> 1 + ri + rd)

data Polinomio a = X
                | Cte a
                | Suma (Polinomio a) (Polinomio a)
                | Prod (Polinomio a) (Polinomio a)

evaluar :: Num a => a -> Polinomio a -> a
evaluar x p = case p of
    X -> x
    Cte k -> k
    Suma p q -> evaluar x p + evaluar x q
    Prod p q -> evaluar x p + evaluar x q

-- definir foldPoli
foldPoli :: b -> (a-> b) -> (b -> b ->b) -> (b->b->b) -> Polinomio a -> b
foldPoli cX cCte cSuma cProd poli = case poli of
    X -> cX
    Cte k -> cCte k
    Suma p q -> cSuma (rec p) (rec q)
    Prod p q -> cProd (rec p) (rec q)
    where rec = foldPoli cX cCte cSuma cProd

-- Otra estructura

data RoseTree a = Rose a [RoseTree a]

-- escribir el esquema de recur estruct para RoseTree
--hojas
--ramas
--tamano
--altura