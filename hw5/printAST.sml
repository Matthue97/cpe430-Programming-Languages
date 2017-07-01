(* printAST.sml Assignment 4 Kevin Yang*)
use "parser.sml";

fun out s = TextIO.output (TextIO.stdOut, s);

fun binaryOperatorString BOP_PLUS = "+"
  | binaryOperatorString BOP_MINUS = "-"
  | binaryOperatorString BOP_TIMES = "*"
  | binaryOperatorString BOP_DIVIDE = "/"
  | binaryOperatorString BOP_MOD = "%"
  | binaryOperatorString BOP_EQ = "=="
  | binaryOperatorString BOP_NE = "!="
  | binaryOperatorString BOP_LT = "<"
  | binaryOperatorString BOP_GT = ">"
  | binaryOperatorString BOP_LE = "<="
  | binaryOperatorString BOP_GE = ">="
  | binaryOperatorString BOP_AND = "&&"
  | binaryOperatorString BOP_OR = "||"
  | binaryOperatorString BOP_COMMA = ","
;

fun unaryOperatorString UOP_NOT = "!"
  | unaryOperatorString UOP_TYPEOF = "typeof "
  | unaryOperatorString UOP_MINUS = "-"
;

fun expressionString (EXP_NUM n) =
   if n < 0 then "-" ^ (Int.toString (~n)) else Int.toString n
  | expressionString (EXP_STRING s) = "\"" ^ (String.toString s) ^ "\""
  | expressionString EXP_TRUE = "true"
  | expressionString EXP_FALSE = "false"
  | expressionString EXP_UNDEFINED = "undefined"
  | expressionString (EXP_ID x) = (String.toString x)
  | expressionString (EXP_BINARY {opr, lft, rht}) =
   "(" ^
       (expressionString lft) ^
       " " ^ (binaryOperatorString opr) ^ " " ^
       (expressionString rht) ^
   ")"
  | expressionString (EXP_UNARY {opr, opnd}) =
   "(" ^
       (unaryOperatorString opr) ^
       (expressionString opnd) ^
   ")"
  | expressionString (EXP_COND {guard, thenExp, elseExp}) =
   "(" ^
       (expressionString guard) ^
       " ? " ^
       (expressionString thenExp) ^
       " : " ^
       (expressionString elseExp) ^
   ")"
   | expressionString (EXP_ASSIGN {cond, assign}) =
    "(" ^
        (expressionString cond) ^ " = " ^ (expressionString assign) ^ ")"
;

fun statementString (ST_EXP {exp}) =
  expressionString exp ^ ";\n" | 
  statementString (ST_IF {guard, thenStatem, elseStatem}) = 
    "if (" ^ (expressionString guard) ^ ")\n" ^ (statementString thenStatem) ^
    "else\n" ^ (statementString elseStatem) | 
  statementString (ST_WHILE {guard, thenWhile}) = 
    "while (" ^ (expressionString guard) ^ ")\n" ^ (statementString thenWhile) |
  statementString (ST_BLOCK {block}) = "{\n" ^ (String.concat (map statementString block)) ^ "}\n" | 
  statementString (ST_PRINT {expr}) = "print " ^ (expressionString expr) ^ ";\n"


;

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