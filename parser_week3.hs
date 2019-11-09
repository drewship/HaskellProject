module JavaishParser where

import qualified Data.Map.Strict as Map
import Debug.Trace

import JavaishScanner

type Program = [ ClassDecl ]

type ClassDecl = ( String, [ ( Type, String ) ], [ MethodDefn ] )

type MethodDefn = ( Type, String, [ ( Type, String ) ], [ Statement ] )

data Type =
    TInt
  | TBool
  | TChar
  | TClassName String
  | TArray Type
  | TFun Type [ Type ]
  deriving ( Show, Eq )

data Statement =
    StmtDeclare Type String
  | StmtAssign String Expression
  | StmtReturn Expression
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

parseProgram :: [ TokenPos ] -> [ Statement ]
parseProgram [] = []
parseProgram tokens = stmt : stmts
  where
    ( stmt, afterStmt ) = parseStmt tokens
    stmts = parseProgram afterStmt

parseStmts :: [ TokenPos ] -> ( [ Statement ], [ TokenPos ] )
parseStmts [] = undefined
parseStmts tokens =
  case tokens of
    ( TokSymb CloseBrace, _ ) : afterBrace -> ( [], afterBrace )
    _ -> ( stmt : stmts, afterAll )
  where
    ( stmt, afterStmt ) = parseStmt tokens
    ( stmts, afterAll ) = parseStmts afterStmt

parseStmt :: [ TokenPos ] -> ( Statement, [ TokenPos ] )
parseStmt tokens =
  case afterStmt of
    ( TokSymb Semicolon, _ ) : afterSemi -> ( stmt, afterSemi )
    _ -> undefined -- missing semicolon
  where
    ( torv, afterTorV ) = parseTorV tokens
    ( stmt, afterStmt ) = parseStmt2 torv afterTorV

parseStmt2 :: TORV -> [ TokenPos ] -> ( Statement, [ TokenPos ] )
parseStmt2 torv tokens =
  case tokens of
    ( TokSymb OpAssign, _ ) : afterAssign ->
      ( StmtAssign name exp, afterExp )
      where
        ( exp, afterExp ) = parseTernary afterAssign
        V name = torv
    ( TokVar varName, _ ) : afterVar ->
      case afterVar of
         -- function/method definiton
        ( TokSymb OpenParen, _ ) : afterParen ->
          case afterParams of
            ( TokSymb OpenBrace, _ ) : afterBrace ->
              undefined
            _ -> undefined
          where
            ( params, afterParams ) = parseParamList afterParen
        -- just a variable declaration
        ( TokSymb Semicolon, _ ) : afterSemi ->
          ( StmtDeclare ty varName, afterSemi )
        _ -> undefined
      where
        T ty = torv
    _  -> undefined

parseParamList = undefined

parseTorV :: [ TokenPos ] -> ( TORV, [ TokenPos ] )
parseTorV tokens =
  case tokens of
    ( TokKeyword Int, _ ) : afterInt -> ( T TInt, afterInt )
    ( TokKeyword Boolean, _ ) : afterBool -> ( T TBool, afterBool )
    ( TokVar varName, _ ) : afterVar -> ( V varName, afterVar )
    _ -> ( Debug.Trace.trace $ "WUT " ++ ( show tokens ) ) undefined

parseTernary :: [ TokenPos ] -> ( Expression, [ TokenPos ] )
parseTernary tokens =
  case afterLeft of
    ( TokSymb OpQuestion, _ ) : afterQuestion ->
      case afterMiddle of
        ( TokSymb OpColon, _ ) : afterColon ->
          ( ( ExpTernary expLeft expMiddle expRight ), afterRight )
          where
            ( expRight, afterRight ) = parseTernary afterColon
        _ -> undefined
      where
        ( expMiddle, afterMiddle ) = parseTernary afterQuestion
    _ -> ( expLeft, afterLeft )
  where
    ( expLeft, afterLeft ) = parseBinary tokens


parseBinary :: [ TokenPos ] -> ( Expression, [ TokenPos ] )
parseBinary = foldl parseBinaryGeneric parseF tokenMaps
  where
    parseBinaryGeneric parseHigherPrec tokenMap tokens =
      -- ( Debug.Trace.trace ( "BUILD" ++ show tokenMap ) )
      uncurry parsePrime $ parseHigherPrec tokens
      where
        parsePrime :: Expression -> [ TokenPos ] -> ( Expression, [ TokenPos ] )
        parsePrime expLeft [] = ( expLeft, [] )
        parsePrime expLeft ( first : afterFirst ) =
          -- ( Debug.Trace.trace ( "TOK " ++ ( show ( fst first ) ) ++ ( show tokenMap ) ) )
          case Map.lookup ( fst first ) tokenMap of
            Nothing -> ( expLeft, first : afterFirst )
            Just op -> parsePrime ( ExpBin expLeft op expRight ) afterRight
              where
                ( expRight, afterRight ) = parseHigherPrec afterFirst

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

parseF :: [ TokenPos ] -> ( Expression, [ TokenPos ] )
parseF [] = undefined
parseF ( first : afterFirst ) =
  case first of
    {- single-token expressions -}
    ( TokInt i, _ ) -> ( ExpInt i, afterFirst )
    ( TokTrue, _ )  -> ( ExpBool True, afterFirst )
    ( TokFalse, _ ) -> ( ExpBool False, afterFirst )
    {- end single-token expressions -}

    -- ( TokVar var, _ ) ->
    --   case afterFirst of
    --     -- special case for zero-parameter call
    --     ( TokSymb OpenParen, _ ) : ( TokSymb CloseParen, _ ) : afterClose ->
    --       ( ExpCall ( ExpVar var ) [], afterClose )
    --     -- call with 1+ parameter(s)
    --     ( TokSymb OpenParen, _ ) : afterOpen ->
    --       ( ExpCall ( ExpVar var ) ( paramFirst : params ), afterParams )
    --       where
    --         ( paramFirst, afterParam ) = parseTernary afterOpen
    --         ( params, afterParams ) = parseCallParams afterParam
    --     -- just a variable after all
    --     _ -> ( ExpVar var, afterFirst )

    ( TokSymb OpBang, _ ) -> ( ExpBang expBang, afterExp )
      where
        ( expBang, afterExp ) = parseF afterFirst
    ( TokSymb OpenParen, _ ) ->
      case afterE of
        ( TokSymb CloseParen, _ ) : afterClose -> ( e, afterClose )
        [] -> undefined
        -- ( _, line_num ) -> Left ( "syntax error: line" ++ show line_num)
      where ( e, afterE ) = parseTernary afterFirst
    _ -> ( Debug.Trace.trace $ "parseF" ++ show first ) undefined

parseCallParams :: [ TokenPos ] -> ( [ Expression ], [ TokenPos ] )
parseCallParams tokens =
  case tokens of
    ( TokSymb CloseParen, _ ) : afterParen -> ( [], afterParen )
    ( TokSymb Comma, _ ) : afterComma ->
      ( param : params, afterAll )
      where
        ( param, afterParam ) = parseTernary afterComma
        ( params, afterAll ) = parseCallParams afterParam
    _ -> undefined


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
