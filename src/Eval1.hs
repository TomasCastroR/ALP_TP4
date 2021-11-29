module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v exp) = evalExp exp >>= update v >>= return Skip
stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = stepComm c1 >>= \c' -> return (Seq c' c2)
stepComm (IfThenElse b c1 c2) = evalExp b >>= \r -> if r then return c1 else return c2
stepComm loop@(While b c) = evalExp b >>= \r -> if r then return (Seq c loop) else return Skip

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
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
                             return (div n1 n2)
evalExp (EAssgn var exp) = do n1 <- evalExp exp
                              update var n2
                              return n1
evalExp (ESeq exp1 exp2) = evalExp exp1 >>= evalExp exp2

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


