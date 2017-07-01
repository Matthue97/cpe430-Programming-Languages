(* CPE430 Assignment 6 part 1-3 Kevin Yang*)
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

fun outList (elem::(elems::morelems)) separator print_funct =
   (
    print_funct elem; 
    out separator; 
    outList (elems::morelems) separator print_funct
    ) |
  outList (elem::elems) separator print_funct = 
    (print_funct elem) |
  outList [] separator print_funct = ()
;

fun outParameterList params =
   (
    out "(";
    outList  params ", " outExpression;
    out ")"
   )

and

outSourceElement sourceElem = 
  case sourceElem of
    (STMT {stmt}) => (outStatement stmt) |
    (FUNCT_DECLAR {funct}) => (outFunction funct true)

and 

outFunction (EXP_FUNCT {bind, params, body}) is_declaration =
  (
    (
    if is_declaration = false then 
      out "("
    else
      out ""
    );
    out "function ";
    (
    if isSome bind then 
      outExpression (valOf bind)
    else 
      out ""
    );
    outParameterList params;
    out "\n{\n";
    outList  body "" outSourceElement;
    out "}\n";
    (
      if is_declaration = false then
        out ")"
      else
        out ""
    )
  )

and 

outStatement (ST_EXP {exp}) = (outExpression exp; out ";\n") | 
  outStatement (ST_BLOCK {stmts}) =
    (
      out "{\n"; app outStatement stmts; out "}\n"
    ) | 
  outStatement (ST_IF {guard, th, el}) =
    (
      out "if ("; outExpression guard; out ")\n";
      outStatement th;
      out "else\n"; outStatement el
    ) | 
  outStatement (ST_PRINT {exp}) =
    (
      out "print "; outExpression exp; out ";\n"
    ) | 
  outStatement (ST_WHILE {guard, body}) =
    (
      out "while ("; outExpression guard; out ")\n";
      outStatement body
    ) | 
  outStatement (ST_VAR {var}) =
    (
      out "var ";
      outList  var ", " outExpression;
      out ";\n"
    ) | 
  outStatement (ST_RET {exp}) =
    (
      out "return";
      (
        case exp of
          (SOME e) => 
          (
            out " "; outExpression e
          ) | 
          _ => (out " undefined")
      );
      out ";\n"
    )

and 

outExpression (EXP_ID s) = out s |
  outExpression (EXP_NUM n) =
    out (if n < 0 then "-" ^ (Int.toString (~n)) else Int.toString n) | 
  outExpression (EXP_STRING s) = 
    out ("\"" ^ (String.toString s) ^ "\"") |
  outExpression EXP_TRUE = 
    out "true" |
  outExpression EXP_FALSE = 
    out "false" |
  outExpression EXP_UNDEFINED = 
    out "undefined" |
  outExpression (funct as EXP_FUNCT {bind, params, body}) =
    outFunction funct false | 
  outExpression (EXP_CALL {member, args}) =
   (outExpression member;
    outParameterList args
   ) |
  outExpression (EXP_BINARY {opr, lft, rht}) =
  (
    out "("; 
    outExpression lft; out " "; out (binaryOperatorString opr); out " "; outExpression rht;
    out ")"
  ) |
  outExpression (EXP_UNARY {opr, opnd}) =
  (
    out "(";
    out (unaryOperatorString opr); outExpression opnd;
    out ")"
  ) |
  outExpression (EXP_COND {guard, thenExp, elseExp}) =
  (
    out "(";
    outExpression guard; out " ? "; outExpression thenExp; out " : "; outExpression elseExp;
    out ")"
  ) | 
  outExpression (EXP_ASSIGN {lhs, rhs}) =
  (
    out "(";
    outExpression lhs; out " = "; outExpression rhs;
    out ")"
  ) | 
  outExpression (EXP_VAR {bind, expreval}) =
  (
    outExpression bind;
    case expreval of
      (SOME x) => (out " = "; outExpression x) |
      NONE => (out "")
  )
;

fun outSourceElements [] = () |
  outSourceElements (x::xs) =
  (
    outSourceElement x; 
    outSourceElements xs
  )
;

fun printAST (PROGRAM {elems}) =
   outList elems "" outSourceElement
;