import JavaishScanner
import JavaishParser
import JavaishTypeChecker
import JavaishInterpreter

-- from type test167 = typeCheck $ fst $ parseR $ scanStartMain "4 + 5"

-- Awesome testing:
prog001 = "class Dog { int main() { int x ; if( true ){ x = 2; } else if( false ) {x = 4; } else { x = 8; } return x ;}}"
test001 = typecheck prog001
test101 = interpret prog001

{-
class Dog {
  int main() {
    int x ;
    if( false ){
      x = 2;
    }
    else if( false ) {
      x = 4;
    }
    else {
      x = 8;
    }
    return x ;
  }
}
-}


prog002 = "class Dog { int main() { int x ; if( false ){ x = 1; } else if( false ) {x = 4; } return x ;}}"
test002 = typecheck prog002
test102 = interpret prog002


{-
Prog002
  class Dog {
    int main() {
      int x ;
      if( false ){
        x = 2;
      }
      else if( false ) {
        x = 4;
      }
      return x ;
    }
  }
-}

prog003 = "class Dog { int main() { int x; if( true ){ if ( false ){ x = 5; } else { x = 1; }} else if( false ) { x = 7; } else{ x = 6; } return x;}}"
test003 = typecheck prog002
test103 = interpret prog002

{-
PROGRAM 003
  class Dog {
    int main() {
     int x;
     if( true ){
        if ( false ){
          x = 5;
        }
        else {
          x = 1;
        }
      }
    else if( false ) {
      x = 7;
    }
    else{
      x = 6;
    }
    return x;
    }
  }

-}

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
