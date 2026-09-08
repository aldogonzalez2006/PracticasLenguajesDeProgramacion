module Interp where

import Grammars
import Data.List (nub)

-- RETO 3: sustitucion nominal que evita captura

-- freeVars e : las variables que aparecen LIBRES en e
-- (no ligadas por ningun let/let* que las contenga)
freeVars :: ASA -> [String]
freeVars (Id x)          = [x]
freeVars (Num _)         = []
freeVars (Boolean _)     = []
freeVars (And es)        = concatMap freeVars es
freeVars (Or es)         = concatMap freeVars es
freeVars (Add es)        = concatMap freeVars es
freeVars (Sub es)        = concatMap freeVars es
freeVars (Mul es)        = concatMap freeVars es
freeVars (Div es)        = concatMap freeVars es
freeVars (Lt es)         = concatMap freeVars es
freeVars (Gt es)         = concatMap freeVars es
freeVars (Le es)         = concatMap freeVars es
freeVars (Ge es)         = concatMap freeVars es
freeVars (Expt e1 e2)    = freeVars e1 ++ freeVars e2
freeVars (EqP e1 e2)     = freeVars e1 ++ freeVars e2
freeVars (Not e)         = freeVars e
freeVars (Add1 e)        = freeVars e
freeVars (Sub1 e)        = freeVars e
freeVars (ZeroP e)       = freeVars e
-- let: las variables ligadas NO son libres en el cuerpo,
-- pero SI en las expresiones de los bindings (se evaluan en el exterior)
freeVars (Let bs cuerpo) =
  let xs = map fst bs
  in  (concatMap (freeVars . snd) bs) ++ filter (`notElem` xs) (freeVars cuerpo)
-- let*: cada binding liga a los posteriores y al cuerpo
freeVars (LetStar bs cuerpo) = freeVarsLetStar bs cuerpo
  where
    freeVarsLetStar [] c = freeVars c
    freeVarsLetStar ((x, e) : rest) c =
      freeVars e ++ filter (/= x) (freeVarsLetStar rest c)

-- names e : TODOS los nombres que aparecen en e (ligados o libres)
names :: ASA -> [String]
names (Id x)          = [x]
names (Num _)         = []
names (Boolean _)     = []
names (And es)        = concatMap names es
names (Or es)         = concatMap names es
names (Add es)        = concatMap names es
names (Sub es)        = concatMap names es
names (Mul es)        = concatMap names es
names (Div es)        = concatMap names es
names (Lt es)         = concatMap names es
names (Gt es)         = concatMap names es
names (Le es)         = concatMap names es
names (Ge es)         = concatMap names es
names (Expt e1 e2)    = names e1 ++ names e2
names (EqP e1 e2)     = names e1 ++ names e2
names (Not e)         = names e
names (Add1 e)        = names e
names (Sub1 e)        = names e
names (ZeroP e)       = names e
names (Let bs cuerpo) =
  concatMap (\(x, e) -> x : names e) bs ++ names cuerpo
names (LetStar bs cuerpo) =
  concatMap (\(x, e) -> x : names e) bs ++ names cuerpo

-- freshName ns : genera un nombre que NO este en ns
freshName :: [String] -> String
freshName ns = head [ "z" ++ show i | i <- [0 :: Int ..], ("z" ++ show i) `notElem` ns ]

-- ---------------------------------------------------------------------------
-- Renombrado (alpha-conversion) de un ligador en una expresion.
-- renombrar e viejo nuevo : reemplaza TODAS las apariciones de la variable
-- 'viejo' por 'nuevo' en e, tanto las ligadas (ligadores) como las libres.
-- Se usa para evitar captura: si al sustituir x:=s entro a un let que liga
-- 'viejo' y 'viejo' aparece libre en s, renombro 'viejo' -> 'nuevo' (fresco).
-- ---------------------------------------------------------------------------
renombrar :: ASA -> String -> String -> ASA
renombrar (Id y) viejo nuevo
  | y == viejo = Id nuevo
  | otherwise  = Id y
renombrar (Num n) _ _        = Num n
renombrar (Boolean b) _ _    = Boolean b
renombrar (And es) v n       = And (map (\e -> renombrar e v n) es)
renombrar (Or es) v n        = Or (map (\e -> renombrar e v n) es)
renombrar (Add es) v n       = Add (map (\e -> renombrar e v n) es)
renombrar (Sub es) v n       = Sub (map (\e -> renombrar e v n) es)
renombrar (Mul es) v n       = Mul (map (\e -> renombrar e v n) es)
renombrar (Div es) v n       = Div (map (\e -> renombrar e v n) es)
renombrar (Lt es) v n        = Lt (map (\e -> renombrar e v n) es)
renombrar (Gt es) v n        = Gt (map (\e -> renombrar e v n) es)
renombrar (Le es) v n        = Le (map (\e -> renombrar e v n) es)
renombrar (Ge es) v n        = Ge (map (\e -> renombrar e v n) es)
renombrar (Expt e1 e2) v n   = Expt (renombrar e1 v n) (renombrar e2 v n)
renombrar (EqP e1 e2) v n    = EqP (renombrar e1 v n) (renombrar e2 v n)
renombrar (Not e) v n        = Not (renombrar e v n)
renombrar (Add1 e) v n       = Add1 (renombrar e v n)
renombrar (Sub1 e) v n       = Sub1 (renombrar e v n)
renombrar (ZeroP e) v n      = ZeroP (renombrar e v n)
renombrar (Let bs cuerpo) v n =
  Let (map (\(y, e) -> (renombrarVar y v n, renombrar e v n)) bs)
      (renombrar cuerpo v n)
renombrar (LetStar bs cuerpo) v n =
  LetStar (map (\(y, e) -> (renombrarVar y v n, renombrar e v n)) bs)
          (renombrar cuerpo v n)

-- renombrarVar y viejo nuevo : renombra el ligador y si y == viejo
renombrarVar :: String -> String -> String -> String
renombrarVar y v n
  | y == v    = n
  | otherwise = y

-- ---------------------------------------------------------------------------
-- sust e x s = e[x := s]
-- Sustitucion nominal que evita captura.
--
-- REGLA DE ORO: al sustituir x:=s en un let/let*, la variable x se sustituye
-- en las EXPRESIONES de los bindings (ahi es libre, se evaluan en el exterior)
-- y en el CUERPO SOLO si x NO esta ligada por ese let/let*.
-- ---------------------------------------------------------------------------
sust :: ASA -> String -> ASA -> ASA
sust (Id y) x s
  | y == x    = s
  | otherwise = Id y
sust (Num n) _ _        = Num n
sust (Boolean b) _ _    = Boolean b
sust (And es) x s       = And (map (\e -> sust e x s) es)
sust (Or es) x s        = Or (map (\e -> sust e x s) es)
sust (Add es) x s       = Add (map (\e -> sust e x s) es)
sust (Sub es) x s       = Sub (map (\e -> sust e x s) es)
sust (Mul es) x s       = Mul (map (\e -> sust e x s) es)
sust (Div es) x s       = Div (map (\e -> sust e x s) es)
sust (Lt es) x s        = Lt (map (\e -> sust e x s) es)
sust (Gt es) x s        = Gt (map (\e -> sust e x s) es)
sust (Le es) x s        = Le (map (\e -> sust e x s) es)
sust (Ge es) x s        = Ge (map (\e -> sust e x s) es)
sust (Expt e1 e2) x s   = Expt (sust e1 x s) (sust e2 x s)
sust (EqP e1 e2) x s    = EqP (sust e1 x s) (sust e2 x s)
sust (Not e) x s        = Not (sust e x s)
sust (Add1 e) x s       = Add1 (sust e x s)
sust (Sub1 e) x s       = Sub1 (sust e x s)
sust (ZeroP e) x s      = ZeroP (sust e x s)

-- Caso Let (simultaneo).
-- Sustituyo x:=s en las EXPRESIONES de los bindings (ahi x es libre).
-- En el cuerpo: si x esta ligada por este let, NO sustituyo (esta ligada).
-- Evito captura: si x NO esta ligada y s tiene libres que chocan con
-- ligadores de este let, renombro esos ligadores en el Let completo
-- (bindings y cuerpo) ANTES de sustituir en el cuerpo.
sust (Let bs cuerpo) x s =
  let xs = map fst bs
      bs' = map (\(y, e) -> (y, sust e x s)) bs
  in if x `elem` xs
     then Let bs' cuerpo
     else let Let bsR cR = renombrarConflictos xs (Let bs' cuerpo)
          in Let bsR (sust cR x s)
  where
    -- renombrar los ligadores de bs (dados en xs) que estan libres en s
    renombrarConflictos xs' e =
      foldr (\y acc -> renombrar acc y (freshName (names acc ++ names s)))
            e (filter (`elem` freeVars s) xs')

-- Caso LetStar (secuencial).
-- Cada binding liga a los posteriores y al cuerpo.
-- Recorro la lista recursivamente con GUARDAS (mas legible que if anidados):
--   * x == y            : x esta ligada por este binding, no sigo sustituyendo
--                          en el resto ni en el cuerpo (el let* interno ya no
--                          ve la x externa). Sustituyo solo en e (x es libre).
--   * y libre en s      : hay riesgo de captura; renombro y -> z (fresco) en
--                          el resto y el cuerpo, y sigo recursando.
--   * otherwise         : sin conflicto; sustituyo en e y sigo recursando.
sust (LetStar bs cuerpo) x s = sustLetStar bs cuerpo
  where
    sustLetStar [] c = LetStar [] (sust c x s)
    sustLetStar ((y, e) : rest) c
      | x == y = LetStar ((y, sust e x s) : rest) c
      | y `elem` freeVars s =
          let z = freshName (names (LetStar rest c) ++ names s)
              LetStar restR cR = renombrar (LetStar rest c) y z
              LetStar rest' c' = sustLetStar restR cR
          in LetStar ((z, sust e x s) : rest') c'
      | otherwise =
          let LetStar rest' c' = sustLetStar rest c
          in LetStar ((y, sust e x s) : rest') c'

-- ---------------------------------------------------------------------------
-- sustMany e bs : aplica TODAS las sustituciones de bs a la vez sobre e
-- (simultaneo: ninguna sustitucion afecta a las demas)
-- ---------------------------------------------------------------------------
sustMany :: ASA -> [Binding] -> ASA
sustMany e [] = e
sustMany e bs = go e
  where
    go (Id y) = case lookup y bs of
                  Just v  -> v
                  Nothing -> Id y
    go (Num n)          = Num n
    go (Boolean b)      = Boolean b
    go (And es)         = And (map go es)
    go (Or es)          = Or (map go es)
    go (Add es)         = Add (map go es)
    go (Sub es)         = Sub (map go es)
    go (Mul es)         = Mul (map go es)
    go (Div es)         = Div (map go es)
    go (Lt es)          = Lt (map go es)
    go (Gt es)          = Gt (map go es)
    go (Le es)          = Le (map go es)
    go (Ge es)          = Ge (map go es)
    go (Expt e1 e2)     = Expt (go e1) (go e2)
    go (EqP e1 e2)      = EqP (go e1) (go e2)
    go (Not e)          = Not (go e)
    go (Add1 e)         = Add1 (go e)
    go (Sub1 e)         = Sub1 (go e)
    go (ZeroP e)        = ZeroP (go e)
    -- al entrar a un let, las variables que liga NO se sustituyen en el cuerpo
    go (Let bs' cuerpo') =
      let xs = map fst bs'
          bs'' = map (\(y, e) -> (y, go e)) bs'
          bsRest = filter (\(y, _) -> y `notElem` xs) bs
      in Let bs'' (sustMany cuerpo' bsRest)
    go (LetStar bs' cuerpo') =
      let xs = map fst bs'
          bs'' = map (\(y, e) -> (y, go e)) bs'
          bsRest = filter (\(y, _) -> y `notElem` xs) bs
      in LetStar bs'' (sustMany cuerpo' bsRest)

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.

-- toNum: extrae el entero de un Num, o Nothing si no es numero
toNum :: ASA -> Maybe Int
toNum (Num n) = Just n
toNum _       = Nothing

bigStep :: ASA -> Maybe ASA
-- atomos: ya son valores
bigStep (Num n)     = Just (Num n)
bigStep (Boolean b) = Just (Boolean b)
-- variable libre: no hay entorno, error
bigStep (Id x)      = Nothing

-- operadores unarios
bigStep (Add1 e) = do
  n <- toNum =<< bigStep e
  return (Num (n + 1))
-- Sub1: aritmetica natural, satura en 0 (0 - 1 = 0)
bigStep (Sub1 e) = do
  n <- toNum =<< bigStep e
  return (Num (max 0 (n - 1)))
bigStep (ZeroP e) = do
  n <- toNum =<< bigStep e
  return (Boolean (n == 0))
-- not: en MiniLisp++ (como en Scheme), solo #f es falso;
-- 0 y cualquier numero no-cero son VERDADEROS.
-- Entonces (not x) es #t solo si x es #f.
bigStep (Not e) = do
  v <- bigStep e
  case v of
    Boolean False -> return (Boolean True)
    _             -> return (Boolean False)

-- operadores n-arios (listas de argumentos)
bigStep (Add es) = do
  ns <- mapM (toNum =<<) (map bigStep es)
  return (Num (sum ns))
-- Sub: resta n-aria con aritmetica natural (satura en 0)
bigStep (Sub es) = do
  ns <- mapM (toNum =<<) (map bigStep es)
  case ns of
    []      -> return (Num 0)
    (n:ns') -> return (Num (max 0 (n - sum ns')))
bigStep (Mul es) = do
  ns <- mapM (toNum =<<) (map bigStep es)
  return (Num (product ns))
bigStep (Div es) = do
  ns <- mapM (toNum =<<) (map bigStep es)
  case ns of
    []      -> return (Num 0)
    (n:ns') -> if 0 `elem` ns'
               then Nothing
               else return (Num (n `div` product ns'))
bigStep (And es) = do
  bs <- mapM (\(Boolean b) -> Just b) =<< mapM bigStep es
  return (Boolean (and bs))
bigStep (Or es) = do
  bs <- mapM (\(Boolean b) -> Just b) =<< mapM bigStep es
  return (Boolean (or bs))
-- comparaciones n-arias: cadena de comparaciones consecutivas
-- Lt [a,b,c] = a < b && b < c
bigStep (Lt es) = do
  ns <- mapM (toNum =<<) (map bigStep es)
  case ns of
    []      -> return (Boolean True)
    [_]     -> return (Boolean True)
    _       -> return (Boolean (and (zipWith (<) ns (tail ns))))
bigStep (Gt es) = do
  ns <- mapM (toNum =<<) (map bigStep es)
  case ns of
    []      -> return (Boolean True)
    [_]     -> return (Boolean True)
    _       -> return (Boolean (and (zipWith (>) ns (tail ns))))
bigStep (Le es) = do
  ns <- mapM (toNum =<<) (map bigStep es)
  case ns of
    []      -> return (Boolean True)
    [_]     -> return (Boolean True)
    _       -> return (Boolean (and (zipWith (<=) ns (tail ns))))
bigStep (Ge es) = do
  ns <- mapM (toNum =<<) (map bigStep es)
  case ns of
    []      -> return (Boolean True)
    [_]     -> return (Boolean True)
    _       -> return (Boolean (and (zipWith (>=) ns (tail ns))))

-- operadores binarios
bigStep (Expt e1 e2) = do
  a <- toNum =<< bigStep e1
  b <- toNum =<< bigStep e2
  return (Num (a ^ b))
-- EqP: igualdad, solo entre valores del MISMO tipo.
-- El patron (Num a, Num b) desarma la estructura; las GUARDAS deciden
-- si devolver True o False segun el valor de a == b.
bigStep (EqP e1 e2) = do
  v1 <- bigStep e1
  v2 <- bigStep e2
  case (v1, v2) of
    (Num a, Num b)         | a == b    -> return (Boolean True)
                           | otherwise -> return (Boolean False)
    (Boolean a, Boolean b) | a == b    -> return (Boolean True)
                           | otherwise -> return (Boolean False)
    _                                  -> Nothing

-- let: simultaneo
-- 1) evaluar TODAS las expresiones de los bindings en el exterior
-- 2) sustituir simultaneamente en el cuerpo
-- 3) evaluar el cuerpo ya sustituido
-- Si hay variables duplicadas en los bindings, es un error (Nothing).
bigStep (Let bs cuerpo) =
  let xs = map fst bs
  in if length xs /= length (nub xs)
     then Nothing
     else do
       vs <- mapM (\(_, e) -> bigStep e) bs
       let sustituciones = zip xs vs
       bigStep (sustMany cuerpo sustituciones)

-- let*: secuencial, regla directa
-- base: sin bindings, evaluar el cuerpo
-- paso: evaluar e1, sustituir x1:=v1 en TODO el let* restante, recursar
bigStep (LetStar [] cuerpo) = bigStep cuerpo
bigStep (LetStar ((x, e) : bs) cuerpo) = do
  v <- bigStep e
  bigStep (sust (LetStar bs cuerpo) x v)
