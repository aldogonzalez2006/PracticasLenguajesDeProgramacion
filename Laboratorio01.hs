module Laboratorio01 where

-- Reto 1:
-- En el archivo md de Leslie ya nos da la formula de la distancia euclidiana
-- donde recibe las coordenadas x,y y nos devuelve la distancia como un double
-- lo adaptamos a haskell que maneja la notación infija
distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt (x^2 + y^2)


-- Reto 2:
-- Aqui la pista me la da el pdf de repaso de haskell, el cual nos recuerda las funciones sin escribir ciclos
-- utilizamos filter, map y sum
-- 1. Filtramos los elementos pares con 'even'.
-- 2. Elevamos al cuadrado cada uno con 'map (^2)'.
-- 3. Sumamos el resultado con 'sum'.
sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum (map(^2) (filter even xs))

-- Reto 3:
-- Es casi el mismo ejercicio que en el pdf solo que aqui se nos pide aplicar la función 3 veces,
-- por lo que con nuestros conocimientos de recursión podemos adaptar el ejemplo a lo que se nos pide
aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

-- Reto 4
varianza2 :: Double -> Double -> Double

-- Reto 5
clasificaTemperatura :: Int -> String

-- Reto 6
intercala :: a -> [a] -> [a]

-- Reto 7
data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int