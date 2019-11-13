module JavaishTypeChecker where

import Control.Monad
import qualified Data.Map.Strict as Map

import JavaishParser

-- a helper function that's similar to Boolean "all", but monad flavored
allM :: Monad m => ( a -> m Bool ) -> [ a ] -> m Bool
allM f [] = return True
allM f ( thing : things ) = do
  truefalse <- f thing
  if truefalse then
    allM f things
  else
    return False

type EnvMap = Map.Map String Type

-- method name |-> ( return type, parameters )
type MethodDescr = Map.Map String ( Type, [ ( Type, String ) ] )

-- ( field name |-> field type, method desc map )
type ClassDescr = ( Map.Map String Type, MethodDescr )

-- ( class name |-> class description )
type ClassMap = Map.Map String ClassDescr

typeCheckProg :: [ ClassDecl ] -> Maybe String
typeCheckProg classes = secondPassCheckClasses classMap classes
  where
    classMap = firstPassBuildClassMap classes
-- typeCheckProg = helper Map.empty
--   where
--     helper :: EnvMap -> [ Statement ] -> Bool
--     helper env statements =
--       case statements of
--         [] -> True
--         ( stmt : stmts ) -> checked && helper envNext stmts
--           where
--             ( checked, envNext ) = typeCheckStmt env stmt

firstPassBuildClassMap :: [ ClassDecl ] -> ClassMap
firstPassBuildClassMap classDecls = foldl helper Map.empty classDecls
  where
    -- TODO: Raise error on duplicate classes, fields or methods
    helper map ( className, fields, methods ) =
      Map.insert className ( fieldMap, methodMap ) map
      where
        fieldMap =
          foldl ( \fm ( fieldType, fieldName ) -> Map.insert fieldName fieldType fm )
                Map.empty fields
        methodMap =
          foldl ( \mm ( typeRet, name, params, _ ) -> Map.insert name ( typeRet, params ) mm )
                Map.empty methods

-- Just "error" or Nothing for no error
secondPassCheckClasses :: ClassMap -> [ ClassDecl ] -> Maybe String
secondPassCheckClasses classMap classDecls =
  case classDecls of
    [] -> Nothing
    classDecl : classes ->
      case checkClass classMap classDecl of
        Just errMsg -> Just errMsg
        Nothing -> secondPassCheckClasses classMap classes

checkClass :: ClassMap -> ClassDecl -> Maybe String
checkClass classMap ( name, _, methodsAll ) = helper methodsAll
  where
    helper [] = Nothing
    helper ( method : methods ) =
      case checkMethod classMap name method of
        Just errMsg -> Just errMsg
        Nothing -> helper methods

checkMethod :: ClassMap -> String -> MethodDefn -> Maybe String
checkMethod classMap nameClass ( typeRet, name, params, body ) =
  case foldM ( typeCheckStmt typeRet classMap ) initialEnv body of
    Left errMsg -> Just errMsg
    Right _ -> Nothing
  where
    initialEnv = foldl ( \env ( t, n ) -> Map.insert n t env ) justThis params
    justThis = Map.fromList [ ( "this", TClassName nameClass ) ]

typeCheckStmt :: Type -> ClassMap -> EnvMap -> Statement -> Either String EnvMap
typeCheckStmt typeRet classMap env stmt =
  case stmt of
    StmtDeclare ty var ->
      case Map.lookup var env of
        Nothing -> Right( Map.insert var ty env )
        _ -> Left "redeclaration of variable"
    StmtAssign lhs rhs -> do
      typeOfRHS <- typeOfExp classMap env rhs
      case Map.lookup lhs env of
        Nothing ->
          case Map.lookup lhs fieldMap of
            Nothing -> Left "weird lhs -2"
            Just typeOfLHS ->
              if typeOfRHS == typeOfLHS then
                Right env
              else
                Left "type mismatch in error"
          where
            Just( TClassName thisClass ) = Map.lookup "this" env
            Just( fieldMap, methodMap )  = Map.lookup thisClass classMap
        Just typeOfLHS ->
          if typeOfRHS == typeOfLHS then
            Right env
          else
            Left "type mismatch in error"
    StmtReturn exp -> do
      typeOfRet <- typeOfExp classMap env exp
      if typeOfRet == typeRet then
        Right env
      else
        Left "returning the wrong type"

    StmtWhileBlock wb -> typeCheckWhile typeRet classMap env stmt

    {-StmtForBlock fb -> typeCheckFor typeRet classmap env stmt-}
--     StmtFunDef typeRet name params stmtsBody -> undefined
--       -- do returns in the body match typeRet
--       -- while typechecking body, need to know about params
--         -- make a new env for the body
--       -- add an entry in env name -> TFun typeRet [ param types ]

typeCheckWhile :: Type -> ClassMap -> EnvMap -> Statement -> Either String EnvMap
typeCheckWhile typeRet classMap env stmt = do
  case stmt of
    StmtWhileBlock wb -> typeCheckWhile2 typeRet classMap env wb
    _ -> undefined

typeCheckWhile2 :: Type -> ClassMap -> EnvMap -> [Statement] -> Either String EnvMap
typeCheckWhile2 typeRet classMap env (first:rest) =
  case first of
    StmtWhile exp body -> do
      expType <- typeOfExp classMap env exp
      if (expType == TBool) then
        typeCheckWhileBody typeRet classMap env body
      else
        Left "Not a bool"

typeCheckWhileBody :: Type -> ClassMap -> EnvMap -> [Statement] -> Either String EnvMap
typeCheckWhileBody _ _ env [] = Right env
typeCheckWhileBody typeRet classMap env (first:rest) = do
  result <- typeCheckStmt typeRet classMap env first
  typeCheckWhileBody typeRet classMap env rest

{-typeCheckFor :: Type -> ClassMap -> EnvMap -> Statement -> Either String EnvMap
typeCheckFor typeRet classMap env stmt = do
  case stmt of
    StmtForBlock wb -> typeCheckFor2 typeRet classMap env wb
    _ -> undefined

typeCheckFor2 :: Type -> ClassMap -> EnvMap -> [Statement] -> Either String EnvMap
typeCheckFor2 typeRet classMap env (first:rest) =
  case first of
    StmtFor stmt1 exp exp2 body -> do
      expType <- typeOfExp classMap env exp
      if (expType == TBool) then
        typeCheckWhileBody typeRet classMap env body
      else
        Left "Not a bool"

typeCheckForBody :: Type -> ClassMap -> EnvMap -> [Statement] -> Either String EnvMap
typeCheckForBody _ _ env [] = Right env
typeCheckForBody typeRet classMap env (first:rest) = do
  result <- typeCheckStmt typeRet classMap env first
  typeCheckForBody typeRet classMap env rest-}

typeOfExp :: ClassMap -> EnvMap -> Expression -> Either String Type
typeOfExp classMap env exp =
  case exp of
    ExpInt _ -> Right TInt
    ExpBool _ -> Right TBool
    ExpTernary left middle right -> do
      tLeft   <- typeHere left
      tMiddle <- typeHere middle
      tRight  <- typeHere right
      case ( tLeft, tMiddle == tRight ) of
        ( TBool, True ) -> Right tMiddle
        _ -> Left "type error"
    ExpBin left op right -> do
      tLeft <- typeHere left
      tRight <- typeHere right
      case ( op, tLeft, tRight ) of
        ( OEq, _, _ ) ->
          if tLeft == tRight then
            Right TBool
          else
            Left "type error"
        ( Lt,    TInt, TInt ) -> Right TBool
        ( Plus,  TInt, TInt ) -> Right TInt
        ( Times, TInt, TInt ) -> Right TInt
        _ -> Left "type error"

    ExpBang expBang -> do
      tBang <- typeHere expBang
      case tBang of
        TBool -> Right TBool
        _ -> Left "type error"

    ExpVar var ->
      case Map.lookup var env of
        Just t -> Right t
        Nothing ->
          case Map.lookup var fieldMap of
            Nothing -> Left "weird lhs -1"
            Just t -> Right t
          where
            Just( TClassName thisClass ) = Map.lookup "this" env
            Just( fieldMap, methodMap )  = Map.lookup thisClass classMap

    -- obj . field
    ExpField expObj fieldName -> do
      typeOfObjectExpression <- typeHere expObj
      className <- case typeOfObjectExpression of
        TClassName c -> Right c
        _ -> Left "bad obj"
      fieldMap <- case Map.lookup className classMap of
        Just( f, _ ) -> Right f
        Nothing -> Left "mystery object type"
      case Map.lookup fieldName fieldMap of
        Just t -> Right t
        Nothing -> Left "no field by that name"

    -- new Dog()
    ExpNew name ->
      case Map.lookup name classMap of
        Nothing -> Left "weird class name"
        Just _ -> Right( TClassName name )

    -- method call:  exp . name ( params )
    ExpCall expObj methName actuals -> do
      typeOfObjectExpression <- typeHere expObj
      className <- case typeOfObjectExpression of
        TClassName c -> Right c
        _ -> Left "bad call"
      methodMap <- case Map.lookup className classMap of
        Just( _, m ) -> Right m
        Nothing -> Left "mystery object type"
      ( typeRet, formals ) <- case Map.lookup methName methodMap of
        Just( t, f ) -> Right( t, f )
        Nothing -> Left "no method by that name"
      params <- if length actuals == length formals then
                  Right( zip formals actuals )
                else
                  Left "param list length mismatch"
      let checkParam ( ( formalType, _ ), actual ) = do
            actualType <- typeHere actual
            if actualType == formalType then
              Right True
            else
              Left "param type"

      allParamsCheck <- allM checkParam params
      if allParamsCheck then
        Right typeRet
      else
        Left "bad params"

    -- _ -> Left "unexpected expression kind"
  where
    typeHere = typeOfExp classMap env
