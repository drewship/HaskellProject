import JavaishScanner
import JavaishParser
import JavaishTypeChecker
import JavaishInterpreter

-- from type test167 = typeCheck $ fst $ parseR $ scanStartMain "4 + 5"

-- Awesome testing:

test14 = scanStartMain "short ] fooooo ,,, throw this (in) the trash"
test15 = scanStartMain "short ] \n fooooo ,,, throw \n this (in) the trash"
test16 = scanStartMain "short ] \n \"fooooo ,\",, throw \n this (in) the trash"

test17 = scanStartMain "4564567 { if else \n (in) the trash"
test111 = parseBinary $ scanStartMain "42"
test112 = parseBinary $ scanStartMain "42 + 17"
test113 = parseBinary $ scanStartMain "42 + 17 + 3"
test114 = parseBinary $ scanStartMain "42 + ( 17 + 3 )"
test115 = parseBinary $ scanStartMain "42 + 17 * 3"
test116 = parseBinary $ scanStartMain "42 * 17 + 3"
test117 = parseBinary $ scanStartMain "42 * ( 17 - 3 ) / 2"
test118 = parseBinary $ scanStartMain "42 2"
test119 = parseBinary $ scanStartMain "42 + 2 )"
test120 = parseBinary $ scanStartMain "42 + ( 2"
test121 = parseBinary $ scanStartMain "(3+4)*5"
test122 = parseBinary $ scanStartMain "4 + 5 < 6"
test123 = parseBinary $ scanStartMain "4 < 5 + 6"
test124 = parseBinary $ scanStartMain "4 ? 5 : 6"
test125 = parseBinary $ scanStartMain "4 ? 5 : 6 ? 7 : 8"

prog001 = "class Dog { boolean main() { if(false){  int x; x = 1;} else if( false ) { int x; x = 2; } else{  int x; x = 3; } return ( x > 2);}}"
test001 = typecheck prog001
test101 = interpret prog001

prog002 = "class Dog { boolean main() { if( true ){  int x; if ( false ){ x = 5; } else { x = 1; }} else if( false ) { int x; x = 5; } else{  int x; x = 5; } return ( x > 2);}}"
test002 = typecheck prog002
test102 = interpret prog002


prog1 = "class Dog { }"
prog2 = "class Dog { int legs; }"
prog3 = "class Dog { int legs; boolean shaggy; }"
prog4 = "class Dog { int legs; boolean shaggy; int woof() { return 4; } }"
prog5 = prog4 ++ "class Cat { Dog friend; boolean meow( Dog d ) { int x; Cat y; x = d.woof(); return false; } }"

parse = parseProgram . scanStartMain

-- newExp = ExpNew "Dog"

typecheck str = do
  classDecls <- parse str
  case typeCheckProg classDecls of
    Just errMsg -> Left errMsg
    Nothing -> Right ()

interpret str = do
  classDecls <- parse str
  evalProg classDecls
