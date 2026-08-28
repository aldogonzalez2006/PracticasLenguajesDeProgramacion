{
module Grammars where

import Lexer (Token(..), lexer)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      nat             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMul }
      '/'             { TokenDiv }
      "and"           { TokenAnd }
      "or"            { TokenOr }
      "not"           { TokenNot }
      "add1"          { TokenAdd1 }
      "sub1"          { TokenSub1 }
      "zero?"         { TokenZeroP }
      "expt"          { TokenExpt }
      '<'             { TokenLT }
      '>'             { TokenGT }
      "<="            { TokenLE }
      ">="            { TokenGE }
      "eq"            { TokenEq }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

ASA : nat                      { Num $1 }
    | bool                     { Boolean $1 }

    -- Happy es el generador de parsers, asi que su estructura será de la izq: la estructura que busco en el texto en MiniLisp
    -- lado derecho: código haskell para construir el ASA.
    -- $ sirve para reconocer la posición de cada token, comenzando en 1 de izq a derecha, 


    -- RETO 2: El pdf nos dice que MiniLisp se divide en 3 tipos de operadores
    -- UnOp:not, add1, sub1, zero? -> Sintaxis (op expr)
    -- BinOp: expt(exponencial), eq(igualdad) -> Sintaxis: (op expr1 expr2)
    -- N-arios: nd, or, +, -, *, /, <, >, <=, >= -> Sintaxis: (op expr1 expr2 expr3 ...)
    -- Con esto podemos ver que existe cierta "exlusividad de operadores" ( + 1) no es valido asi como (expt 5 6 7)


    -- Operadores n-arios (2 o mas argumentos)

    | '(' "and" DosOMas ')'    { And $3 }
    | '(' "or" DosOMas ')'     { Or $3 }
    | '(' '+' DosOMas ')'      { Add $3 }
    | '(' '-' DosOMas ')'      { Sub $3 }
    | '(' '*' DosOMas ')'      { Mul $3 }
    | '(' '/' DosOMas ')'      { Div $3 }
    | '(' '<' DosOMas ')'      { Lt $3 }
    | '(' '>' DosOMas ')'      { Gt $3 }
    | '(' "<=" DosOMas ')'     { Le $3 }
    | '(' ">=" DosOMas ')'     { Ge $3 }

    --Operadores estrictamente binarios (exactamente 2, de ser 1 o <3 será no válido)
    | '(' "expt" ASA ASA ')'   { Expt $3 $4 }
    | '(' "eq" ASA ASA ')'     { EqP $3 $4 }

    -- Operadores unarios (exactamente 1)
    | '(' "not" ASA ')'        { Not $3 }
    | '(' "add1" ASA ')'       { Add1 $3 }
    | '(' "sub1" ASA ')'       { Sub1 $3 }
    | '(' "zero?" ASA ')'      { ZeroP $3 }

-- RETO 3: no terminal para dos o mas argumentos
-- DosOMas : ASA ASA            (2 argumentos)
--         | ASA DosOMas        (3 o mas, recursivo)

DosOMas : ASA ASA              { [$1, $2] }
        | ASA DosOMas          { $1 : $2 }

{
parseError :: [Token] -> a
parseError toks = error ("Parse error: " ++ show toks)

data ASA
  = Num Int
  | Boolean Bool
  | And [ASA]
  | Or [ASA]
  | Add [ASA]
  | Sub [ASA]
  | Mul [ASA]
  | Div [ASA]
  | Lt [ASA]
  | Gt [ASA]
  | Le [ASA]
  | Ge [ASA]
  | Expt ASA ASA
  | EqP ASA ASA
  | Not ASA
  | Add1 ASA
  | Sub1 ASA
  | ZeroP ASA
  deriving (Eq, Show)
}