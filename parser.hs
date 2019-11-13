module JavaishParser where

import qualified Data.Map.Strict as Map
import Debug.Trace

import JavaishScanner

type Program = [ ClassDecl ]

-- ( class name, fields, methods )
type ClassDecl = ( String, [ ( Type, String ) ], [ MethodDefn ] )

-- ( return type, method name, parameters, body )
type MethodDefn = ( Type, String, [ ( Type, String ) ], [ Statement ] )

data Type =
    TInt
  | TBool
  | TChar
  | TClassName String
  | TArray Type
  deriving ( Show, Eq )

data Statement =
    StmtDeclare Type String
  | StmtAssign String Expression
  | StmtReturn Expression
  | StmtWhileBlock [Statement]
  | StmtWhile Expression [Statement]
  | StmtForBlock [Statement]
  | StmtFor Statement Expression Expression [Statement]
  deriving Show

data Expression =
    ExpInt Integer
  | ExpBool Bool
  | ExpVar String
  | ExpTernary Expression Expression Expression
  | ExpBin Expression Operator Expression
  | ExpBang Expression
  | ExpNew String
  | ExpField Expression String
  | ExpCall Expression String [ Expression ]
  deriving Show

data Operator = Plus | Minus | Times | Divide | Mod | ShiftLeft | ShiftRightLog | ShiftRightArith | Gt | Lt | GtEq | LtEq | OEq | ONe | BitAnd | BitXor | BitOr | LogAnd | LogOr
  deriving Show

data TORV = T Type | V String

parseProgram :: [ TokenPos ] -> Either String [ ClassDecl ]
parseProgram [] = Right []
parseProgram tokens = do
  ( classDecl, afterClass ) <- parseClassDecl tokens
  remainingClasses <- parseProgram afterClass
  return( classDecl : remainingClasses )

-- parseProgram :: [ TokenPos ] -> [ Statement ]
-- parseProgram [] = []
-- parseProgram tokens = stmt : stmts
--   where
--     ( stmt, afterStmt ) = parseStmt tokens
--     stmts = parseProgram afterStmt

parseClassDecl :: [ TokenPos ] -> Either String ( ClassDecl, [ TokenPos ] )
parseClassDecl tokens =
  case tokens of
    ( TokKeyword Class, _ ) : ( TokVar v, _ ) : ( TokSymb OpenBrace, _ ) : afterOpenBrace -> do
      ( fields, methods, afterFieldsAndMethods ) <- parseFieldsAndMethods afterOpenBrace
      return( ( v, fields, methods ), afterFieldsAndMethods )
    _ -> Left "not classy"

parseFieldsAndMethods :: [ TokenPos ] ->
  Either String ( [ ( Type, String ) ], [ MethodDefn ], [ TokenPos ] )
parseFieldsAndMethods tokens =
  case tokens of
    ( TokSymb CloseBrace, _ ) : afterClose -> Right( [], [], afterClose )
    _ -> do
      ( ty, afterType ) <- parseType tokens
      ( name, afterName ) <- case afterType of
        ( TokVar v, _ ) : a -> Right( v, a )
        _ -> Left "not field or method. expecting variable"
      ( f, m, afterFieldOrMethod ) <- case afterName of
        ( TokSymb OpenParen, _ ) : afterOpen -> do
          ( method, afterMethod ) <- parseMethod ty name afterOpen
          Right( [], [ method ], afterMethod )
        ( TokSymb Semicolon, _ ) : afterSemi ->
          Right( [ ( ty, name ) ], [], afterSemi )
        _ -> Left "not field or method. expecting open paren or semicolon"
      ( fs, ms, afterFieldsAndMethods ) <- parseFieldsAndMethods afterFieldOrMethod
      return( f ++ fs, m ++ ms, afterFieldsAndMethods )

parseType :: [ TokenPos ] -> Either String ( Type, [ TokenPos ] )
parseType tokens =
  case tokens of
    ( TokKeyword Int, _ )     : afterInt  -> Right ( TInt, afterInt )
    ( TokKeyword Boolean, _ ) : afterBool -> Right ( TBool, afterBool )
    ( TokVar name, _ )        : afterName -> Right ( TClassName name, afterName )
    _ -> Left "not type"

-- Have already seen "Type Name ("
parseMethod :: Type -> String -> [ TokenPos ] -> Either String ( MethodDefn, [ TokenPos ] )
parseMethod returnType name tokens = do
  ( formals, afterFormals ) <- case tokens of
    ( TokSymb CloseParen, _ ) : afterClose -> Right( [], afterClose )
    _ -> do
      ( firstType, afterType ) <- parseType tokens
      case afterType of
        ( TokVar firstName, _ ) : afterName -> do
          ( remainingFormals, after ) <- parseFormals afterName
          Right( ( firstType, firstName ) : remainingFormals, after )
        _ -> Left "bad formals"

  afterOpen <- case afterFormals of
    ( TokSymb OpenBrace, _ ) : a -> Right a
    _ -> Left "bad method"

  ( body, afterBody ) <- parseStmts afterOpen

  Right( ( returnType, name, formals, body ), afterBody )

-- Have already parsed the first formal, so expecting ", T N, T N, T N )"
parseFormals :: [ TokenPos ] -> Either String ( [ ( Type, String ) ], [ TokenPos ] )
parseFormals tokens =
  case tokens of
    ( TokSymb CloseParen, _ ) : afterClose -> Right( [], afterClose )
    ( TokSymb Comma, _ ) : afterComma -> do
      ( formalType, afterType ) <- parseType afterComma
      case afterType of
        ( TokVar name, _ ) : afterName -> do
          ( formals, afterFormals ) <- parseFormals afterName
          Right( ( formalType, name ) : formals, afterFormals )
    _ -> Left "bad formals"

parseStmts :: [ TokenPos ] -> Either String ( [ Statement ], [ TokenPos ] )
parseStmts [] = undefined
parseStmts tokens =
  case tokens of
    ( TokSymb CloseBrace, _ ) : afterBrace -> Right( [], afterBrace )
    _ -> do
      ( stmt, afterStmt ) <- parseStmt tokens
      ( stmts, afterAll ) <- parseStmts afterStmt
      Right( stmt : stmts, afterAll )

parseStmt :: [ TokenPos ] -> Either String ( Statement, [ TokenPos ] )
parseStmt tokens =
  case tokens of
    ( TokKeyword Return, _ ) : afterReturn -> do
      ( exp, afterExp ) <- parseTernary afterReturn
      case afterExp of
        ( TokSymb Semicolon, _ ) : afterSemi -> Right( StmtReturn exp, afterSemi )
        _ -> Left "missing semicolon after return"
    (TokKeyword While, _) : afterWhile -> do
      (stmts, afterStmts) <- parseWhile afterWhile
      Right (stmts, afterStmts)
    {-}(TokKeyword For, _) : afterFor -> do
      (stmts, afterStmts) <- parseFor afterFor
      Right (stmts, afterStmts)-}
    _ -> do
      ( torv, afterTorV ) <- parseTorV tokens
      ( stmt, afterStmt ) <- parseStmt2 torv afterTorV
      case afterStmt of
        ( TokSymb Semicolon, _ ) : afterSemi -> Right( stmt, afterSemi )
        _ -> Left "missing semicolon"

parseStmt2 :: TORV -> [ TokenPos ] -> Either String ( Statement, [ TokenPos ] )
parseStmt2 torv tokens =
  case tokens of
    ( TokSymb OpAssign, _ ) : afterAssign -> do
        ( exp, afterExp ) <- parseTernary afterAssign
        let V name = torv
        Right( StmtAssign name exp, afterExp )

    ( TokVar varName, _ ) : afterVar -> Right( StmtDeclare ty varName, afterVar )
      where
        ty = case torv of
          T t -> t
          V v -> TClassName v

    _  -> Left "not a statement"

parseTorV :: [ TokenPos ] -> Either String ( TORV, [ TokenPos ] )
parseTorV tokens =
  case tokens of
    ( TokKeyword Int, _ )     : afterInt  -> Right ( T TInt, afterInt )
    ( TokKeyword Boolean, _ ) : afterBool -> Right ( T TBool, afterBool )
    ( TokVar varName, _ )     : afterVar  -> Right ( V varName, afterVar )
    ( t : _ ) -> Left ( "not type or var:" ++ show t )
    [] -> Left "end of input; expecting type or var"

parseWhile :: [TokenPos] -> Either String (Statement, [TokenPos])
parseWhile tokens = do
  (whileExp, afterExp) <- parseF tokens
  case afterExp of
    ((TokSymb OpenBrace, _): afterBrace) -> do
      (whileStmts, afterWhileStmts) <- whileHelper afterBrace
      let whileStmt = StmtWhile whileExp whileStmts
      case afterWhileStmts of
        _ -> Right (whileBlock, afterWhileStmts)
          where
            whileBlock = StmtWhileBlock (whileStmt:[])
    _ -> undefined

whileHelper :: [TokenPos] -> Either String ([Statement], [TokenPos])
whileHelper [] = undefined
whileHelper tokens =
  case tokens of
    (TokSymb CloseBrace, _) : afterBrace -> Right ([], afterBrace)
    _ -> do
      (stmt, afterStmt) <- parseStmt tokens
      (stmts, afterAll) <- parseStmts afterStmt
      Right (stmt:stmts, afterAll)

{-parseFor :: [TokenPos] -> Either String (Statement, [TokenPos])
parseFor tokens = do
  (forExp, afterExp) <- parseF tokens
  case afterExp of
    ((TokSymb OpenBrace, _): afterBrace) -> do
      (forStmts, afterForStmts) <- forHelper afterBrace
      let forStmt = StmtFor  s forExp e2 forStmts where
        s =
        e2 =
      case afterForStmts of
        _ -> Right (forBlock, afterForStmts)
          where
            forBlock = StmtForBlock (forStmt:[])
    _ -> undefined

forHelper :: [TokenPos] -> Either String ([Statement], [TokenPos])
forHelper [] = undefined
forHelper tokens =
  case tokens of
    (TokSymb CloseBrace, _) : afterBrace -> Right ([], afterBrace)
    _ -> do
      (stmt, afterStmt) <- parseStmt tokens
      (stmts, afterAll) <- parseStmts afterStmt
      Right (stmt:stmts, afterAll)

parseForParams :: [TokenPos] -> Either String (Statement, Expression, Expression, [TokenPos])
parseForParams [] = undefined
case s:sc1::tokens of
  (s _, e1, e2, afterParams)
  _ -> "invalid for loop"-}


parseTernary :: [ TokenPos ] -> Either String ( Expression, [ TokenPos ] )
parseTernary tokens = do
  ( expBool, afterBool ) <- parseBinary tokens
  case afterBool of
    ( TokSymb OpQuestion, _ ) : afterQuestion -> do
      ( expTrue, afterTrue ) <- parseTernary afterQuestion
      case afterTrue of
        ( TokSymb OpColon, _ ) : afterColon -> do
          ( expFalse, afterFalse ) <- parseTernary afterColon
          Right( ( ExpTernary expBool expTrue expFalse ), afterFalse )
        _ -> Left "bad ?:"

    _ -> Right( expBool, afterBool )

type ExpParser = [ TokenPos ] -> Either String ( Expression, [ TokenPos ] )

parseBinary :: [ TokenPos ] -> Either String ( Expression, [ TokenPos ] )
parseBinary = foldl parseBinaryGeneric parseF tokenMaps
  where
    parseBinaryGeneric :: ExpParser -> ( Map.Map Token Operator ) -> ExpParser
    parseBinaryGeneric parseHigherPrec tokenMap tokens =
      -- ( Debug.Trace.trace ( "BUILD" ++ show tokenMap ) )
      case parseHigherPrec tokens of
        Left errMsg -> Left errMsg
        Right( exp, remaining ) -> parsePrime exp remaining
      where
        parsePrime :: Expression -> [ TokenPos ] -> Either String ( Expression, [ TokenPos ] )
        parsePrime expLeft [] = Right( expLeft, [] )
        parsePrime expLeft ( first : afterFirst ) =
          -- ( Debug.Trace.trace ( "TOK " ++ ( show ( fst first ) ) ++ ( show tokenMap ) ) )
          case Map.lookup ( fst first ) tokenMap of
            Nothing -> Right( expLeft, first : afterFirst )
            Just op -> do
              ( expRight, afterRight ) <- parseHigherPrec afterFirst
              parsePrime ( ExpBin expLeft op expRight ) afterRight

    tokenMaps =
      map Map.fromList
        [ [ ( TokSymb OpTimes, Times ), ( TokSymb OpDiv, Divide ), ( TokSymb OpMod, Mod ) ],
          [ ( TokSymb OpPlus, Plus ), ( TokSymb OpMinus, Minus ) ],
          [ ( TokSymb OpShiftLeft, ShiftLeft ), ( TokSymb OpShiftRightLog, ShiftRightLog ), ( TokSymb OpShiftRightArith, ShiftRightArith ) ],
          [ ( TokSymb OpLt, Lt ), ( TokSymb OpLtEq, LtEq ), ( TokSymb OpGt, Gt ), ( TokSymb OpGtEq, GtEq ) ],
          [ ( TokSymb OpEq, OEq ), ( TokSymb OpNotEq, ONe ) ],
          [ ( TokSymb OpBitAnd, BitAnd ) ],
          [ ( TokSymb OpBitXor, BitXor ) ],
          [ ( TokSymb OpBitOr, BitOr ) ],
          [ ( TokSymb OpLogAnd, LogAnd ) ],
          [ ( TokSymb OpLogOr, LogOr ) ]
        ]

parseF :: [ TokenPos ] -> Either String ( Expression, [ TokenPos ] )
parseF [] = Left "expect expression; got no tokens"
parseF ( first : afterFirst ) =
  case first of
    {- single-token expressions -}
    ( TokInt i, _ )   -> parseDot ( ExpInt i ) afterFirst
    ( TokTrue, _ )    -> parseDot ( ExpBool True ) afterFirst
    ( TokFalse, _ )   -> parseDot ( ExpBool False ) afterFirst
    ( TokVar var, _ ) -> parseDot ( ExpVar var ) afterFirst
    {- end single-token expressions -}

    ( TokKeyword New, _ ) -> case afterFirst of
      ( TokVar name, _ ) : ( TokSymb OpenParen, _ ) : ( TokSymb CloseParen, _ ) : afterNew ->
        parseDot ( ExpNew name ) afterNew

    ( TokSymb OpBang, _ ) -> do
      ( expBang, afterExp ) <- parseF afterFirst
      Right( ExpBang expBang, afterExp )

    ( TokSymb OpenParen, _ ) -> do
      ( e, afterE ) <- parseTernary afterFirst
      case afterE of
        ( TokSymb CloseParen, _ ) : afterClose -> parseDot e afterClose
        [] -> Left "expecting close paren"

    _ -> ( Debug.Trace.trace $ "parseF" ++ show first ) undefined

-- already saw "EXP"
parseDot :: Expression -> [ TokenPos ] -> Either String ( Expression, [ TokenPos ] )
parseDot expBefore tokens =
  case tokens of
    ( TokSymb Dot, _ ) : afterDot ->
      case afterDot of
        ( TokVar name, _ ) : ( TokSymb OpenParen, _ ) : ( TokSymb CloseParen, _ ) : afterClose ->
          parseDot ( ExpCall expBefore name [] ) afterClose
        ( TokVar name, _ ) : ( TokSymb OpenParen, _ ) : afterOpen -> do
          ( actualFirst, afterActual ) <- parseTernary afterOpen
          ( actuals, afterActuals ) <- parseActuals afterActual
          parseDot ( ExpCall expBefore name ( actualFirst : actuals ) ) afterActuals
        ( TokVar name, _ ) : afterName ->
          parseDot ( ExpField expBefore name ) afterName

    _ -> Right( expBefore, tokens )

parseActuals :: [ TokenPos ] -> Either String ( [ Expression ], [ TokenPos ] )
parseActuals tokens =
  case tokens of
    ( TokSymb CloseParen, _ ) : afterParen -> Right( [], afterParen )
    ( TokSymb Comma, _ ) : afterComma -> do
      ( param, afterParam ) <- parseTernary afterComma
      ( params, afterAll ) <- parseActuals afterParam
      Right( param : params, afterAll )
    _ -> Left "bad actuals"


{-
16	[] . ()	access array element access object member parentheses	left to right
15	++ --	unary post-increment unary post-decrement	not associative
14	++ -- + - ! ~	unary pre-increment unary pre-decrement unary plus unary minus unary logical NOT unary bitwise NOT	right to left
13	() new	cast object creation	right to left

12	* / %	multiplicative	left to right
11	+ - +	additive string concatenation	left to right
10	<< >> >>>	shift	left to right
9	< <= > >= instanceof	relational	not associative
8	== !=	equality	left to right
7	&	bitwise AND	left to right
6	^	bitwise XOR	left to right
5	|	bitwise OR	left to right
4	&&	logical AND	left to right
3	||	logical OR	left to right

2	?:	ternary	right to left
1	 =   +=   -= *=   /=   %= &=   ^=   |= <<=  >>= >>>=	assignment	right to left
-}
