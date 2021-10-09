module BAE where

import Test.HUnit
import Data.List

-- | Name. Tipo que representa el nombre de variables.
type Name = String

-- | BAE. Tipo que representa las funciones aritmetico-booleanas.
data BAE = V Name 
         | N Int 
         | B Bool
         | Suc BAE
         | Pre BAE
         | Plus BAE BAE
         | Prod BAE BAE
         | Neg BAE
         | Conj BAE BAE
         | Disy BAE BAE
         | Gt BAE BAE
         | Lt BAE BAE
         | Equi BAE BAE
         | Ift BAE BAE BAE
         | LetE Name BAE BAE

-- | Sust. Tipo que representa una sustitución sobre BAE.
type Sust = (Name,BAE)

-- | Instancia de la clase Show para BAE.
instance Show BAE where
  show (V s) = "var[" ++ s ++ "]"
  show (N x) = "num[" ++ show x ++ "]"
  show (B b) = "bool[" ++ show b ++ "]"
  show _     = error "D:"


instance Eq BAE where
  (==) (V s) (V s') = s == s'
  (==) (V _) _ = False
  (==) (N s) (N s') = s == s'
  (==) (N _) _ = False
  (==) (B a) (B a') = a == a'
  (==) (B _) _ = False
  (==) a b = error "D:"

-- substituion. Función que regresa la sustitución de una BAE.
substitution :: BAE -> Sust -> BAE
substitution = error "D:"

-- | fv. Función que regresa las  variables libres de una expresión de
-- BAE.
fv :: BAE -> [Name]
fv (V x)      = [x]
fv (N n)      = []
fv (B b)      = []
fv (Plus a b) = fv a `union` fv b

-- | eval1.   Función que implementa un  paso en la evaluación  de una
-- expresión de BAE.
eval1 :: BAE -> BAE
eval1 (N n)           = (N n)
eval1 (V x)           = (V x)
eval1 (B b)           = (B b)
eval1 (Suc (N n))     = N (n+1)
eval1 (Suc n)         = Suc (eval1 n)
eval1 (Ift (B b) t f)
  | b                 = t
  | otherwise         = f
eval1 (Ift b t f)     = Ift (eval1 b) t f

-- | evals.  Función que implementa la  cerradura reflexiva-transitiva
-- de una evaluación.
evals :: BAE -> BAE
evals = error "D:"
