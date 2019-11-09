module JavaishScanner where
-- module JavaishScanner( TokenPos, scanStartMain ) where

import Data.Char
import qualified Data.Map.Strict as Map

data Keyword = Abstract | Assert | Boolean | Break | Byte | Case | Catch | Char | Class | Const | Continue | Default | Do | Double | Else | Enum | Extends | Final | Finally | Float | For | If | Goto | Implements | Import | Instanceof | Int | Interface | Long | Native | New | Package | Private | Protected | Public | Return | Short | Static | Strictfp | Super | Switch | Synchronized | This | Throw | Throws | Transient | Try | Void | Volatile | While | Underscore
  deriving ( Eq, Ord, Show )

keyword_map = Map.fromList [
  ( "abstract", Abstract ), ( "assert", Assert ), ( "boolean", Boolean ), ( "break", Break ), ( "byte", Byte ), ( "case", Case ), ( "catch", Catch ), ( "char", Char ), ( "class", Class ), ( "const", Const ), ( "continue", Continue ), ( "default", Default ), ( "do", Do ), ( "double", Double ), ( "else", Else ), ( "enum", Enum ), ( "extends", Extends ), ( "final", Final ), ( "finally", Finally ), ( "float", Float ), ( "for", For ), ( "if", If ), ( "goto", Goto ), ( "implements", Implements ), ( "import", Import ), ( "instanceof", Instanceof ), ( "int", Int ), ( "interface", Interface ), ( "long", Long ), ( "native", Native ), ( "new", New ), ( "package", Package ), ( "private", Private ), ( "protected", Protected ), ( "public", Public ), ( "return", Return ), ( "short", Short ), ( "static", Static ), ( "strictfp", Strictfp ), ( "super", Super ), ( "switch", Switch ), ( "synchronized", Synchronized ), ( "this", This ), ( "throw", Throw ), ( "throws", Throws ), ( "transient", Transient ), ( "try", Try ), ( "void", Void ), ( "volatile", Volatile ), ( "while", While ), ( "_", Underscore )
  ]

data Symbol = OpAssign | OpGt | OpLt | OpBang | OpTilde | OpQuestion | OpColon | OpArrow | OpEq | OpGtEq | OpLtEq | OpNotEq | OpLogAnd | OpLogOr | OpIncr | OpDecr | OpPlus | OpMinus | OpTimes | OpDiv | OpBitAnd | OpBitOr | OpBitXor | OpMod | OpShiftLeft | OpShiftRightArith | OpShiftRightLog | OpPlusAssign | OpMinusAssign | OpTimesAssign | OpDivAssign | OpBitAndAssign | OpBitOrAssign | OpBitXorAssign | OpModAssign | OpShiftLeftAssign | OpShiftRightArithAssign | OpShiftRightLogAssign | OpenParen | CloseParen | OpenBracket | CloseBracket | OpenBrace | CloseBrace | Semicolon | Comma | Dot | DotDotDot | At | ColonColon
  deriving ( Eq, Ord, Show )

all_symbols = [ ( "+", OpPlus ), ( "-", OpMinus ), ( "*", OpTimes ), ( "/", OpDiv ),
  ( "&", OpBitAnd ), ( "|", OpBitOr ), ( "^", OpBitXor ), ( "%", OpMod ),
  ( "<<", OpShiftLeft ), ( ">>", OpShiftRightArith ), ( ">>>", OpShiftRightLog ),
  ( "&&", OpLogAnd ), ( "||", OpLogOr ), ( "!", OpBang ), ( "~", OpTilde ),
  ( "=", OpAssign ), ( "+=", OpPlusAssign ), ( "-=", OpMinusAssign ), ( "*=", OpTimesAssign ), ( "/=", OpDivAssign ),
  ( "&=", OpBitAndAssign ), ( "|=", OpBitOrAssign ), ( "^=", OpBitXorAssign ), ( "%=", OpModAssign ),
  ( "<<=", OpShiftLeftAssign ),( ">>=", OpShiftRightArithAssign ), ( ">>>=" , OpShiftRightLogAssign ),
  ( "++", OpIncr ), ( "--", OpDecr ),
  ( "==", OpEq ),  ( "!=", OpNotEq ),  ( ">", OpGt ), ( "<", OpLt ), ( ">=", OpGtEq ), ( "<=", OpLtEq ),
  ( "?", OpQuestion ), ( ":", OpColon ),
  ( "(", OpenParen ), ( ")", CloseParen ), ( "{", OpenBrace ), ( "}", CloseBrace ), ( "[", OpenBracket ), ( "]", CloseBracket ),
  ( ";", Semicolon ), ( ",", Comma ), ( ".", Dot ),
  ( "@", At ), ( "::", ColonColon ), ( "->", OpArrow ),  ( "...", DotDotDot ) ]

symbols_of_len l = filter ( \( str, sym ) -> length str == l ) all_symbols
symb4_map = Map.fromList $ symbols_of_len 4
symb3_map = Map.fromList $ symbols_of_len 3
symb2_map = Map.fromList $ symbols_of_len 2
symb1_map = Map.fromList $ symbols_of_len 1

data Token = TokKeyword Keyword | TokSymb Symbol | TokVar String | TokStr String | TokInt Integer | TokChar Char | TokNull | TokTrue | TokFalse
  deriving ( Eq, Ord, Show )

type TokenPos = ( Token, Integer )

isJavaIdentifierStart c = isAlpha c || c == '_'
isJavaIdentifierPart c = isAlphaNum c || c == '_'
integralFromDigit = fromIntegral . digitToInt

-- line_num = 0   "variables" in Haskell are constant

scanStartMain = scanStart 1

scanStart :: Integer -> String -> [ TokenPos ]
scanStart _ [] = []
scanStart line_num ( first : rest ) =
  case first of
    -- line terminators are the worst
    '\n' -> scanStart ( line_num + 1 ) rest
    '\r' ->
      case rest of
        []           -> []
        '\n' : rest2 -> scanStart ( line_num + 1 ) rest2
        _            -> scanStart ( line_num + 1 ) rest
    -- the literal actual physical worst

    '"' -> scanString line_num "" rest
    -- || first == '.' floats ?!?!?
    _ | isNumber first -> scanNum line_num ( integralFromDigit first ) rest
    _ | isJavaIdentifierStart first -> scanSawLetters line_num [ first ] rest
    _ -> case scanSymbol ( first : rest ) of
           Just ( symbol, after_symbol ) ->
             ( TokSymb symbol, line_num ) : ( scanStartSame after_symbol )
           Nothing -> scanStartSame rest
  where
    scanStartSame = scanStart line_num

scanString line_num str_so_far input_stream =
  case input_stream of
    [] -> [ ( TokStr str, line_num ) ]
    ( first : rest ) ->
      if first == '"' then
        ( TokStr str, line_num ) : ( scanStart line_num rest )
      else
        scanString line_num ( first : str_so_far ) rest
  where
    str = reverse str_so_far

scanSawLetters :: Integer -> String -> String -> [ TokenPos ]
scanSawLetters line_num chars_backwards input_stream =
  case input_stream of
    [] -> [ finish ]
    first : rest ->
      if isJavaIdentifierPart first then
        scanSawLetters line_num ( first : chars_backwards ) rest
      else
        finish : ( scanStart line_num ( first : rest ) )
  where
    finish =
      if chars == "true" then
        ( TokTrue, line_num )
      else if chars == "false" then
        ( TokFalse, line_num )
      else case Map.lookup chars keyword_map of
             Just kw -> ( TokKeyword kw, line_num )
             Nothing -> ( TokVar chars, line_num )
    chars = reverse chars_backwards

scanNum :: Integer -> Integer -> String -> [ TokenPos ]
scanNum line_num num_so_far input_stream =
  case input_stream of
    [] -> [ ( TokInt num_so_far, line_num ) ]
    first : rest ->
      if isNumber first then
        scanNum line_num ( 10 * num_so_far + integralFromDigit first ) rest
      else
        ( TokInt num_so_far, line_num ) : ( scanStart line_num ( first : rest ) )

scanSymbol :: String -> Maybe ( Symbol, String )
scanSymbol input_stream =
  case ( s4, s3, s2, s1 ) of
    ( Just s, _, _, _ ) -> Just ( s, drop 4 input_stream )
    ( _, Just s, _, _ ) -> Just ( s, drop 3 input_stream )
    ( _, _, Just s, _ ) -> Just ( s, drop 2 input_stream )
    ( _, _, _, Just s ) -> Just ( s, drop 1 input_stream )
    _ -> Nothing
  where
    s1 = Map.lookup ( take 1 input_stream ) symb1_map
    s2 = Map.lookup ( take 2 input_stream ) symb2_map
    s3 = Map.lookup ( take 3 input_stream ) symb3_map
    s4 = Map.lookup ( take 4 input_stream ) symb4_map
