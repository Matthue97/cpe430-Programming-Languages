(* printAST.sml Assignment 4 Kevin Yang*)
use "parser.sml";

fun printExpr exp = 
  case exp of
  (EXP_BINARY bin) => (
      print "(";
      printExpr (#lft bin);
      (
        case (#opr bin) of
        (BOP_EQ) => (print " == ") |
        (BOP_NE) => (print " != ") |
        (BOP_PLUS) => (print " + ") |
        (BOP_MINUS) => (print " - ") |
        (BOP_TIMES) => (print " * ") |
        (BOP_DIVIDE) => (print " / ") |
        (BOP_GE) => (print " >= ") |
        (BOP_GT) => (print " > ") |
        (BOP_LE) => (print " <= ") |
        (BOP_LT) => (print " < ") |
        (BOP_AND) => (print " && ") |
        (BOP_OR) => (print " || ") |
        (BOP_COMMA) => (print " , ") | 
        (BOP_MOD) => (print " % ")
      );
      printExpr (#rht bin);
      print ")"
  ) | 
  (EXP_UNARY un) => (
      print "(";
      (
        case (#opr un) of
        (UOP_MINUS) => (print "-") |
        (UOP_NOT) => (print "!") |
        (UOP_TYPEOF) => (print "typeof ")
      );
      printExpr (#opnd un);
      print ")"
  ) |
  (EXP_COND cond) => (
      print "(";
      printExpr (#guard cond);
      print " ? ";
      printExpr (#thenExp cond);
      print " : ";
      printExpr (#elseExp cond);
      print ")"
  ) |
  (EXP_STRING str) => (
    print ("\"" ^ str ^ "\"")
  ) |
  (EXP_TRUE) => (print "true") |
  (EXP_FALSE) => (print "false") |
  (EXP_NUM num) => (print (Int.toString num)) |
  (EXP_UNDEFINED) => (
    print "undefined"
  )
;

fun printSrc (STMT statement) =
  let
    val ST_EXP expression = (#stmt statement)
  in
    printExpr (#exp expression)
  end
;

fun printList l =
  case l of
  ((x::xs):sourceElement list) => ((printSrc x); (print ";\n"); (printList xs)) |
  ([]) => ()
;

fun printAST (PROGRAM prog) =
  printList (#elems prog)
;