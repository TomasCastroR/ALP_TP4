module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> runStateError m s >>= \(v :!: s') -> runStateError (f v) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:

instance MonadError StateError where
  throw e = StateError (\s -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> case M.lookup v s of
                                  Just n -> Right (n :!: s)
                                  _ -> Left UndefVar)
  update v i = StateError (\s -> Right (() :!: M.insert v i s))
-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = case runStateError (stepCommStar p) initEnv of
            Right (_ :!: s) -> Right s
            Left e -> Left e

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v exp) = do n <- evalExp exp
                          update v n
                          return Skip
stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = stepComm c1 >>= \c' -> return (Seq c' c2)
stepComm (IfThenElse b c1 c2) = evalExp b >>= \r -> if r then return c1 else return c2
stepComm loop@(While b c) = evalExp b >>= \r -> if r then return (Seq c loop) else return Skip

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp (Const n) = return n
evalExp (Var v) = lookfor v
evalExp (UMinus exp) = evalExp exp >>= \n -> return (-n)
evalExp (Plus exp1 exp2) = do n1 <- evalExp exp1
                              n2 <- evalExp exp2
                              return (n1+n2)
evalExp (Minus exp1 exp2) = do n1 <- evalExp exp1
                               n2 <- evalExp exp2
                               return (n1-n2)
evalExp (Times exp1 exp2) = do n1 <- evalExp exp1
                               n2 <- evalExp exp2
                               return (n1*n2)
evalExp (Div exp1 exp2) = do n1 <- evalExp exp1
                             n2 <- evalExp exp2
                             case n2 of
                               0 -> throw DivByZero
                               _ -> return (div n1 n2)
evalExp (EAssgn var exp) = do n <- evalExp exp
                              update var n
                              return n
evalExp (ESeq exp1 exp2) = evalExp exp1 >> evalExp exp2

evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt exp1 exp2) = do b1 <- evalExp exp1
                            b2 <- evalExp exp2
                            return (b1<b2)
evalExp (Gt exp1 exp2) = do b1 <- evalExp exp1
                            b2 <- evalExp exp2
                            return (b1>b2)
evalExp (And exp1 exp2) = do b1 <- evalExp exp1
                             b2 <- evalExp exp2
                             return (b1 && b2)
evalExp (Or exp1 exp2) = do b1 <- evalExp exp1
                            b2 <- evalExp exp2
                            return (b1 || b2)
evalExp (Not exp) = evalExp exp >>= \b -> return (not b)
evalExp (Eq exp1 exp2) = do b1 <- evalExp exp1
                            b2 <- evalExp exp2
                            return (b1 == b2)
evalExp (NEq exp1 exp2) = do b1 <- evalExp exp1
                             b2 <- evalExp exp2
                             return (b1 /= b2)

