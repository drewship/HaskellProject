module JavaishInterpreter where

import JavaishParser
import Control.Monad
import Debug.Trace

import qualified Data.Map.Strict as Map

-- the interpreter only needs to know parameter names and body statements about methods
type MethodStuff = ( [ String ], [ Statement ] )
type ClassMap = Map.Map String ( Map.Map String Type, Map.Map String MethodStuff )

data Value =
    VInt Integer
  | VBool Bool
  | VObj String ( Map.Map String Value )
  | VNull
  deriving Show

type EnvMap = Map.Map String Value

defaultValueForType :: Type -> Value
defaultValueForType t =
  case t of
    TInt -> VInt 0
    TBool -> VBool False
    TClassName _ -> VNull
    _ -> undefined

evalProg :: [ ClassDecl ] -> Either String Value
evalProg classDecls = do
  mainClassName <- case classDecls of
    [] -> Left "nothing!"
    ( n, _, _ ) : _ -> Right n
  mainClassMethods <- case Map.lookup mainClassName classMap of
    Nothing -> Left "Internal error"
    Just( _, m ) -> Right m
  ( mainParams, mainBody ) <- case Map.lookup "main" mainClassMethods of
    Nothing -> Left "no main"
    Just m -> Right m
  case evalStmts classMap Map.empty mainBody of -- TODO: initial environment
    Left errMsg -> Left errMsg
    Right( Left returnVal ) -> Right returnVal
    Right( Right env ) -> Left "missing return"
  where
    classMap = firstPassBuildClassMap classDecls

firstPassBuildClassMap :: [ ClassDecl ] -> ClassMap
firstPassBuildClassMap classDecls = foldl helper Map.empty classDecls
  where
    -- TODO: Raise error on duplicate classes, fields or methods
    helper classMap ( className, fields, methods ) =
      Map.insert className ( fieldMap, methodMap ) classMap
      where
        fieldMap =
          foldl ( \fm ( fieldType, fieldName ) -> Map.insert fieldName fieldType fm )
                Map.empty fields
        methodMap =
          foldl ( \mm ( _, name, params, body ) -> Map.insert name ( map snd params, body ) mm )
                Map.empty methods

evalStmts :: ClassMap -> EnvMap -> [ Statement ] -> Either String ( Either Value EnvMap )
evalStmts classMap initialEnv = foldM ( evalStmt classMap ) ( Right initialEnv )

evalStmt :: ClassMap -> ( Either Value EnvMap ) -> Statement -> Either String ( Either Value EnvMap )
evalStmt classMap envOrRet stmt =
  case envOrRet of
    Left returnValue -> return ( Left returnValue )
    Right env ->
      case stmt of
        StmtDeclare t v -> Right( Right env )
        -- Would be a bit closer to Java:
        -- Map.insert v ( defaultValueForType t ) env

        StmtAssign var e -> do
          val <- evalExp classMap env e
          return ( Right( Map.insert var val env ) )

        StmtReturn exp -> do
          val <- evalExp classMap env exp
          return ( Left val )

        StmtIfBlock choices -> evalIfBlock classMap env stmt

evalIfBlock :: ClassMap -> EnvMap -> Statement -> Either String ( Either Value EnvMap )
evalIfBlock classMap env stmt =
  case stmt of
    StmtIfBlock choices ->
      case result of
        Just selectedStmt ->
          case selectedStmt of
            StmtIf _ body -> evalStmts classMap env body
            StmtElseIf _ body -> evalStmts classMap env body
            StmtElse body -> evalStmts classMap env body
            _ -> undefined
        Nothing -> undefined
      where
        result = chooseStmt classMap env choices

    _ -> undefined

chooseStmt :: ClassMap -> EnvMap -> [Statement] -> Maybe Statement
chooseStmt classMap env [] = Nothing
chooseStmt classMap env ( first : rest ) =
  case first of
    StmtIf exp body-> do
      let express = evalExp classMap env exp
      case express of
        Right (VBool True) -> Just first
        Right (VBool False) -> chooseStmt classMap env rest
        _ -> undefined
    StmtElseIf exp body -> do
      let express = evalExp classMap env exp
      case express of
        Right (VBool True) -> Just first
        Right (VBool False) -> chooseStmt classMap env rest
        _ -> undefined
    StmtElse body -> Just first
    _ -> undefined


evalExp :: ClassMap -> EnvMap -> Expression -> Either String Value
evalExp classMap env exp =
  case exp of
    ExpInt  i -> Right( VInt i )
    ExpBool b -> Right( VBool b )
    ExpBang e -> do
      v <- evalHere e
      let VBool b = v
      Right( VBool( not b ) )
    ExpVar varname ->
      case Map.lookup varname env of
        Just value -> Right( value )
        _ -> ( Debug.Trace.trace $ "yikes" ++ show varname ) undefined
    ExpTernary expTest expIfTrue expIfFalse -> do
      vTest <- evalHere expTest
      case vTest of
        VBool True ->  evalHere expIfTrue
        VBool False -> evalHere expIfFalse
    ExpBin left op right -> do
      valLeft  <- evalHere left
      valRight <- evalHere right
      return ( case ( valLeft, op, valRight ) of
        ( VInt l, Plus,  VInt r ) -> VInt( l + r )
        ( VInt l, Times, VInt r ) -> VInt( l * r )
        ( VInt l, Lt,    VInt r ) -> VBool( l < r )
        ( VInt l, Gt,    VInt r ) -> VBool( l > r )
        _ -> undefined )
    ExpNew className ->
      let Just( fieldMap, _ ) = Map.lookup className classMap in
      return( VObj className ( Map.map defaultValueForType fieldMap ) )
    ExpField objExp fieldName -> do
      v <- evalHere objExp
      let VObj className fieldVals = v
      let Just field = Map.lookup fieldName fieldVals
      return field

    ExpCall expObj methodName expParams -> do
      valObj <- evalHere expObj
      let VObj className fields = valObj
      actuals <- mapM evalHere expParams
      let Just ( _, methodMap ) = Map.lookup className classMap
      let Just ( formals, body ) = Map.lookup methodName methodMap
      let paramMap = Map.fromList ( zip formals actuals )
      let finalMap = Map.insert "this" valObj paramMap
      somedamnthing <- evalStmts classMap finalMap body
      case somedamnthing of
        Left returnedValue -> Right returnedValue
        Right environment -> Left "missing return statement in called function"

  where
    evalHere = evalExp classMap env
