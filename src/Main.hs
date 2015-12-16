module Main where

import qualified Data.Map.Strict as M
import Text.Show.Functions
import Text.Parsec
import Control.Monad (forM_)
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Error as PE
import qualified Text.Parsec.Language as L


--- Types
type Name = String

type Env = M.Map Name Value

data Expr = ELit Double
          | EVar Name
          | ELam [Name] Expr
          | EApp Expr [Expr]
          | ERLet [(Name,Expr)] Expr
            deriving Show

--- Interpreter

data Value = VConst Double
           | VClosure [Name] Expr Env
           | VPrim ([Value]->Value)
             deriving Show
                      
eval :: Expr -> Env -> Value

eval (ELit d) env = VConst d

eval (EVar n) env
    = case n `M.lookup` env of
        Nothing -> error $ "Unknown variable " ++ show n
        Just v -> v 

eval (ELam formals body) env = VClosure formals body env

eval (EApp func args) env
  = case eval func env of
      VClosure formals body clEnv
          -> let newbinds = M.fromList
                            [(n,eval a env)|(n,a) <- zip formals args]
                 env' = newbinds `M.union` clEnv in
             eval body env'
      VPrim f
          -> f [eval a env|a<-args]
      otherwise
          -> error $ "Not a procedure: "++show func

eval (ERLet bindings body) env
     = let benv = M.fromList [(n,eval v env')|(n,v)<-bindings]
           env' = benv `M.union` env in
       eval body env'

adder :: [Value] -> Value
adder = VConst . adder'
    where
      adder' [] = 0
      adder' ((VConst d):xs) = d+adder' xs
      adder' _ = error "Can add only numbers"

defEnv :: Env
defEnv = M.fromList $ [("add",VPrim adder)]

evalDef :: Expr -> Value
evalDef e = eval e defEnv

--- Parser
exprP :: Parsec String () Expr
exprP = numberP
  <|> varP
  <|> (parensP (letExprP
               <|> lamExprP
               <|> appExprP))

numberP = ELit <$> floatP
varP = EVar <$> identP

bindExprP = parensP $ do
  n <- identP
  e <- exprP
  return $ (n,e)

letExprP = do
  reservedP "let"
  binders <- parensP $ many1 bindExprP
  body <- exprP
  return $ ERLet binders body

lamExprP = do
  reservedP "lambda"
  formals <- parensP $ many identP
  body <- exprP
  return $ ELam formals body

appExprP = do
  op <- exprP
  args <- many exprP
  return $ EApp op args
  
floatP = do
  res <- P.naturalOrFloat lexer
  return $ case res of
    Left i -> fromIntegral i
    Right d -> d
    
parensP = P.parens lexer
identP = P.identifier lexer
reservedP = P.reserved lexer

schemeDef
  = L.emptyDef { P.commentLine = ";"
               ,P.identStart = letter <|> char '_'
               ,P.identLetter = letter <|> char '_'
               ,P.reservedNames = ["let","lambda"]
               ,P.caseSensitive = True
               }
               
lexer = P.makeTokenParser schemeDef

-- Driver
main :: IO ()
main = do
  let s = "(let ((f (lambda (x) ( add x a))) (a 2)) (f 4)) ;Test"
  let res = parse exprP "--builtin--" s
  case res of
    Left err -> do
      putStrLn $ "Parse error" ++ show err
    Right expr -> do
      print expr
      print $ evalDef expr
