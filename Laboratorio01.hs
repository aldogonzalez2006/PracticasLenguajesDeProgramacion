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
--Bueno, pues aquí es formulaso, la única variable que tenemos que guardar es la media, la cual la guardamos en
--'media', lo hicimos con let porque es más fácil para nosotros primero tener que guardar la variable que necesitamos
-- y despues usarla en la formula grande que primero declarar la fórmula grande y despues las variables.
varianza2 :: Double -> Double -> Double
varianza2 x y = 
  let media = (x+y)/2 in ((x-media)^2 + (y-media)^2)/2

-- Reto 5
clasificaTemperatura :: Int -> String
--Este solo es un caso de if, en donde tenemos que poner restricciones de cuanto a cuanto se considera cada temperatura
-- y este es perfecto para guardas :)
clasificaTemperatura n 
  | n <= 0        = "frio extremo"
  | n <= 15       = "frio"
  | n <= 24       = "templado"
  | n <= 35       = "calido"
  | otherwise     = "calor extremo"

-- Reto 6
--Aquí lo interesante de este ejercico son los casos base, en donde si la lista que nos dan desde el inicio es vacía, la regresamos tal como esta y ya
--si hay elementos en donde se pueda poner el separador lo hacemos, tomamos la cabeza de la lista, añadimos el separador
-- y retomamos el resto de la lista,  esa lista ya solo tahora si llegamos al punto dondeiene un elemento es nuestro freno de mano o caso base
-- ya que nos dicen que no le agreguemos separador al final, regresamos esa lita tal y como esta y ya.
intercala :: a -> [a] -> [a]
intercala a []        = []  
intercala a [x]       = [x]
intercala a (x:xs)  = x : a : intercala a xs 

-- Reto 7
-- Esta función evalúa de forma recursiva una expresión algebraica con tipos propios, 
-- separando los casos mediante pattern matching: si es un literal numérico solo extrae el valor, 
-- y si es una suma o un producto, evalúa ambos lados de forma independiente y aplica la operación matemática
data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n)          = n
evalua (Suma e1 e2)     = evalua e1 + evalua e2
evalua (Producto e1 e2) = evalua e1 * evalua e2
