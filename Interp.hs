module Interp where

import Grammars
import Data.List (nub)

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun params body
  | length params /= length (nub params) = Nothing -- Para rechazar repetidos
  | otherwise = Just $ foldr Fun body params

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp fun args = Just $ foldl App fun args

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [_] = Nothing
binaryOp op args = Just $ foldl1 op args

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS n) = Just (Id n)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS args) = mapM desugar args >>= binaryOp Add
desugar (SubS args) = mapM desugar args >>= binaryOp Sub
desugar (NotS expr) = Not <$> desugar expr
desugar (FunS params body) = desugar body >>= curryFun params
desugar (AppS fun args) = do
  f <- desugar fun
  as <- mapM desugar args
  curryApp f as
desugar (LetS x e1 e2) = do
  e1' <- desugar e1
  e2' <- desugar e2
  Just (App (Fun x e2') e1')
desugar (LetStarS [] body) = desugar body
desugar (LetStarS ((x, e):bindings) body) = 
  desugar (LetS x e (LetStarS bindings body))

-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv = lookup

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x) = lookupEnv x env
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
bigStep env (Fun x body) = Just (ClosureV x body env)
bigStep env (App fun arg) = do
  funVal <- bigStep env fun
  case funVal of
    ClosureV param body closureEnv -> do
      argVal <- bigStep env arg -- Evaluación ansiosa: pedimos el argumento
      -- Evaluamos el cuerpo en el ambiente de la cerradura extendida
      bigStep ((param, argVal) : closureEnv) body 
    _ -> Nothing
bigStep env (Add e1 e2) = do
  v1 <- bigStep env e1
  v2 <- bigStep env e2
  case (v1, v2) of
    (NumV n1, NumV n2) -> Just (NumV (n1 + n2))
    _ -> Nothing
bigStep env (Sub e1 e2) = do
  v1 <- bigStep env e1
  v2 <- bigStep env e2
  case (v1, v2) of
    (NumV n1, NumV n2) -> Just (NumV (max 0 (n1 - n2))) -- Resta truncada
    _ -> Nothing
bigStep env (Not e) = do
  v <- bigStep env e
  case v of
    BooleanV b -> Just (BooleanV (not b))
    NumV _ -> Just (BooleanV False) -- Todo número es verdadero, su negación es False
    _ -> Nothing
