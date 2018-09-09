module TypeInference (TypingJudgment, Result(..), inferType)

where

import Data.List(intersect)
import Exp
import Type
import Unification

------------
-- Errors --
------------
data Result a = OK a | Error String


--------------------
-- Type Inference --
--------------------
type TypingJudgment = (Env, AnnotExp, Type)


inferType :: PlainExp -> Result TypingJudgment
inferType e = case infer' e 0 of
    OK (_, tj) -> OK tj
    Error s -> Error s


infer' :: PlainExp -> Int -> Result (Int, TypingJudgment)

-- COMPLETAR DESDE AQUI

infer' (VarExp x)     n = OK (n+1, ( E [(x, TVar n)], VarExp x, TVar n)) -- Uso una nueva variable de tipo 
infer' (ZeroExp) n = OK (n, (emptyEnv, ZeroExp, TNat)) 
infer' (SuccExp exp) n = 
	case infer' exp n of
	     OK (n', (env', exp', t') ) ->
	         case mgu [(t', TNat)] of ->
	             UOK subst ->
	                 OK (n', (subst <.> env', subst <.> SuccExp exp', TNat))
	             UError u1 u2 ->
	                 uError u1 u2
	     res@(Error _) -> res
infer' (LamExp s () exp) n = 
	case infer' exp n of 
		OK (n', (env', exp', t')) ->
		    if  elem s (domainE env') then
		        OK (n', (removeE env' s, LamExp s (evalE env')  exp', TFun (evalE env')  t'))
		    else
		        OK (n' + 1, (removeE env' s, LamExp s (TVar n')  exp',  TFun (TVar n') t'))
		res@(Error _) -> res

infer' (AppExp e1 e2) n =
	case infer' e1 n of
		OK (n1, (env1, e1', t1)) ->
		    case infer' e2 n1 of ->
		        OK (n2, (env2, e2', t2)) ->
		            let envGroso = [ (evalE x env1, evalE x env2) | x <- domainE env1, y <- domainE env2 , x == y] in
		                case mgu ([(t1, Tfun t2 (TVar n2))] ++ envGroso) of
		                    UOK subst -> 
		                    	OK (n2 + 1, (joinE (subst <.> env1) (subst <.> env2), subst <.> (AppExp e1 e2), subst <-> (TVar n2)))
		                    UError u1 u2 ->
	                            uError u1 u2	
		        res@(Error _) -> res
		res@(Error _) -> res


--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
