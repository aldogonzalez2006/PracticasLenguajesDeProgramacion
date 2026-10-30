module Laboratorio06 where

-- Laboratorio 06: Transformación de funciones recursivas
-- @author Cano Rodriguez Edgar Yael
-- @author Cervantes Gonzalez Aldo Jael
-- @author Mejia Arroyo Diego
--

-- 1. Potencia --------------------------------------------------------------

potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

-- El acumulador almacena el producto de los factores consumidos 
potenciaCola :: Int -> Int -> Int
potenciaCola base exponente = potenciaCola' base exponente 1
  where
    potenciaCola' :: Int -> Int -> Int -> Int
    potenciaCola' _ 0 acc = acc
    potenciaCola' n m acc = potenciaCola' n (m - 1) (acc * n)

-- CPS: En el caso base m = 0, se aplica la continuación k al valor 1
-- La continuación guarda la multiplicación pendiente (\r -> k (n * r))
potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 k = k 1
potenciaCPS n m k = potenciaCPS n (m - 1) (\r -> k (n * r))


-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = longitudCola' xs 0
  where
    longitudCola' :: [a] -> Int -> Int
    longitudCola' [] acc = acc
    longitudCola' (_ : resto) acc = longitudCola' resto (acc + 1)

-- CPS: Solo se modifica la forma de devolver el resultado evaluando k en el caso base 
longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k 0
longitudCPS (_ : xs) k = longitudCPS xs (\r -> k (r + 1))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

-- En cola se construye con (x : acc) para mantener complejidad O(1) 
reversaCola :: [a] -> [a]
reversaCola xs = reversaCola' xs []
  where
    reversaCola' :: [a] -> [a] -> [a]
    reversaCola' [] acc = acc
    reversaCola' (x : resto) acc = reversaCola' resto (x : acc)

-- CPS: A diferencia del acumulador, la continuación concatena x al final de la lista ya procesada (r ++ [x])
reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x : xs) k = reversaCPS xs (\r -> k (r ++ [x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola xss = concatenaCola' xss []
  where
    concatenaCola' :: [[a]] -> [a] -> [a]
    concatenaCola' [] acc = acc
    concatenaCola' (xs : resto) acc = concatenaCola' resto (acc ++ xs)

-- CPS: Mantiene el orden asociando la sublista a la izquierda (xs ++ r) 
concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (xs : resto) k = concatenaCPS resto (\r -> k (xs ++ r))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

-- El acumulador invierte el orden al hacer (y : x : acc), por lo que requiere invertirse dos veces para preservar el resultado 
intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = reversaCola (intercalaCola' xs ys [])
  where
    intercalaCola' :: [a] -> [a] -> [a] -> [a]
    intercalaCola' [] resto acc = reversaCola resto ++ acc
    intercalaCola' resto [] acc = reversaCola resto ++ acc
    intercalaCola' (x : xs) (y : ys) acc = intercalaCola' xs ys (y : x : acc)

-- CPS: La continuación antepone (x : y : r) directamente sobre el resultado pendiente sin necesidad de invertirse 
intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x : xs) (y : ys) k = intercalaCPS xs ys (\r -> k (x : y : r))

-- ¿Qué conserva el acumulador y qué conserva la continuación?
-- Conservación del acumulador: Guarda el resultado parcial calculado de izquierda a derecha antes de la llamada recursiva.
-- Conservación de la continuación: Guarda el trabajo/contexto pendiente que se evaluará sobre el resultado tras alcanzar el caso base.