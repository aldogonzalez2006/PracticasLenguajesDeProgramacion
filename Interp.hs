module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- Encadenamiento de computos que pueden fallar ----------------------------
--
-- Sustituye a (>>=) de Maybe sin usar do ni case ... of.
-- Se define por dos ecuaciones con patrones:
--
--   encadenar Nothing  _ = Nothing   -- el fallo se propaga
--   encadenar (Just v) k = k v       -- el valor pasa a la continuacion
--
-- Es exactamente el bind de Maybe, pero escrito con ecuaciones.
encadenar :: Maybe a -> (a -> Maybe b) -> Maybe b
encadenar Nothing  _ = Nothing
encadenar (Just v) k = k v

-- RETO 3: desazucarado ----------------------------------------------------

-- FunS [x1,...,xn] cuerpo  ==>  Fun x1 (Fun x2 (... cuerpo))
-- Una lambda sin parametros no tiene representacion en el nucleo.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun []     _      = Nothing
curryFun (x:xs) cuerpo = Just (foldr Fun cuerpo (x:xs))

-- AppS f [a1,...,an]  ==>  App (App f a1) a2 ... an
-- Una aplicacion sin argumentos no tiene representacion en el nucleo.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ []     = Nothing
curryApp f (a:as) = Just (foldl App (App f a) as)

-- [x1,...,xn] con op  ==>  op (op x1 x2) x3 ... xn
-- Un solo operando se acepta tal cual (el test no prueba el caso vacio).
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _  []       = Nothing
binaryOp _  [x]      = Just x
binaryOp op (x:y:xs) = Just (foldl op (op x y) xs)

-- Desazucara las clausulas ordinarias de cond en If anidados a la derecha.
-- Cada condicion, rama y la alternativa final se desazucaran tambien.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond []         elseS = desugar elseS
desugarCond ((c,e):cs) elseS =
  encadenar (desugar c) (\c' ->
  encadenar (desugar e) (\e' ->
  encadenar (desugarCond cs elseS) (\resto ->
  Just (If c' e' resto))))

-- Elimina toda la sintaxis superficial.
--
--   LetS x e c        ==>  App (Fun x c) e
--   LetStarS bs c     ==>  App (Fun x1 (App (Fun x2 ... c) e2)) e1
--   CondS cs elseS    ==>  If anidados a la derecha
--   LetRecS f e c     ==>  App (Fun f c) (App (Id "Y") (Fun f e))
--
-- El nucleo no contiene Let, LetStar ni LetRec: todos se eliminan aqui.
desugar :: SASA -> Maybe ASA
desugar (NumS n)     = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (IdS x)      = Just (Id x)

desugar (FunS xs cuerpo) =
  encadenar (desugar cuerpo) (curryFun xs)

desugar (AppS f args) =
  encadenar (desugar f) (\f' ->
  encadenar (mapM desugar args) (\args' ->
  curryApp f' args'))

desugar (AddS ops) =
  encadenar (mapM desugar ops) (binaryOp Add)

desugar (SubS ops) =
  encadenar (mapM desugar ops) (binaryOp Sub)

desugar (NotS a) =
  encadenar (desugar a) (\a' -> Just (Not a'))

desugar (IfS c t e) =
  encadenar (desugar c) (\c' ->
  encadenar (desugar t) (\t' ->
  encadenar (desugar e) (\e' ->
  Just (If c' t' e'))))

desugar (CondS cs elseS) = desugarCond cs elseS

-- let x = e in c   ==>   (lambda x. c) e
desugar (LetS x e c) =
  encadenar (desugar e) (\e' ->
  encadenar (desugar c) (\c' ->
  Just (App (Fun x c') e')))

-- let* (x1 e1) ... (xn en) in c  ==>  ligaduras anidadas
desugar (LetStarS bs c) = desugarLetStar bs c

-- letrec f = e in c  ==>  (lambda f. c) (Y (lambda f. e))
desugar (LetRecS f e c) =
  encadenar (desugar e) (\e' ->
  encadenar (desugar c) (\c' ->
  Just (App (Fun f c') (App (Id "Y") (Fun f e')))))

-- Auxiliar de let*: ligaduras secuenciales, la primera es la mas externa.
desugarLetStar :: [(Nombre, SASA)] -> SASA -> Maybe ASA
desugarLetStar [] c = desugar c
desugarLetStar ((x,e):bs) c =
  encadenar (desugar e) (\e' ->
  encadenar (desugarLetStar bs c) (\c' ->
  Just (App (Fun x c') e')))

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
-- El nombre lookupEnv no choca con Prelude.lookup (que es otro identificador),
-- y ademas es el nombre que espera TestLaboratorio05.hs.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ []           = Nothing
lookupEnv x ((y,v):resto)
  | x == y    = Just v
  | otherwise = lookupEnv x resto

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict v@(NumV _)         = Just v
strict v@(BooleanV _)     = Just v
strict v@(ClosureV _ _ _) = Just v
strict (ExprV e env)      = encadenar (bigStep env e) strict

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
bigStep :: Env -> ASA -> Maybe Value

bigStep _   (Num n)     = Just (NumV n)
bigStep _   (Boolean b) = Just (BooleanV b)

-- Id devuelve directamente la asociacion encontrada, sin forzarla.
bigStep env (Id x)      = lookupEnv x env

-- Fun produce ClosureV con el ambiente de definicion (alcance estatico).
bigStep env (Fun x cuerpo) = Just (ClosureV x cuerpo env)

-- App exige la posicion de funcion, pero liga el argumento como
-- ExprV argumento ambienteDeLaLlamada (evaluacion diferida).
bigStep env (App f a) =
  encadenar (encadenar (bigStep env f) strict) (\f' ->
  apply f' (ExprV a env))

-- Add, Sub y Not exigen sus operandos.
bigStep env (Add a b) =
  encadenar (encadenar (bigStep env a) strict) (\va ->
  encadenar (encadenar (bigStep env b) strict) (\vb ->
  suma va vb))

bigStep env (Sub a b) =
  encadenar (encadenar (bigStep env a) strict) (\va ->
  encadenar (encadenar (bigStep env b) strict) (\vb ->
  resta va vb))

bigStep env (Not a) =
  encadenar (encadenar (bigStep env a) strict) negacion

-- If exige solamente la condicion y evalua una sola rama.
bigStep env (If c t e) =
  encadenar (encadenar (bigStep env c) strict) (eligeRama env t e)

-- Auxiliares de evaluacion (sin case ... of, con ecuaciones de funcion).

eligeRama :: Env -> ASA -> ASA -> Value -> Maybe Value
eligeRama env t _ (BooleanV True)  = bigStep env t
eligeRama env _ e (BooleanV False) = bigStep env e
eligeRama _   _ _ _                = Nothing

suma :: Value -> Value -> Maybe Value
suma (NumV a) (NumV b) = Just (NumV (a + b))
suma _        _        = Nothing

-- La resta sobre naturales permanece truncada en cero.
resta :: Value -> Value -> Maybe Value
resta (NumV a) (NumV b) = Just (NumV (max 0 (a - b)))
resta _        _        = Nothing

negacion :: Value -> Maybe Value
negacion (BooleanV b) = Just (BooleanV (not b))
negacion _            = Nothing

-- Aplica una cerradura a un argumento (que puede ser un thunk).
apply :: Value -> Value -> Maybe Value
apply (ClosureV x cuerpo env) arg = bigStep ((x, arg) : env) cuerpo
apply _                       _   = Nothing
