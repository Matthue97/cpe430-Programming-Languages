(* CPE430 Assignment 6 part 1-3 Kevin Yang*)
datatype binaryOperator =
     BOP_PLUS
   | BOP_MINUS
   | BOP_TIMES
   | BOP_DIVIDE
   | BOP_MOD
   | BOP_EQ
   | BOP_NE
   | BOP_LT
   | BOP_GT
   | BOP_LE
   | BOP_GE
   | BOP_AND
   | BOP_OR
   | BOP_COMMA
;

datatype unaryOperator =
     UOP_NOT
   | UOP_TYPEOF
   | UOP_MINUS
;

datatype sourceElement =
     STMT of {stmt: statement}
   | FUNCT_DECLAR of {funct: expression}
and

expression =
    EXP_ID of string
  | EXP_NUM of int
  | EXP_STRING of string
  | EXP_TRUE
  | EXP_FALSE
  | EXP_UNDEFINED
  | EXP_BINARY of {opr: binaryOperator, lft: expression, rht: expression}
  | EXP_UNARY of {opr: unaryOperator, opnd: expression}
  | EXP_COND of {guard: expression, thenExp: expression, elseExp: expression}
  | EXP_ASSIGN of {lhs: expression, rhs: expression}
  | EXP_VAR of {bind: expression, expreval: expression option}
  | EXP_FUNCT of {bind: expression option, params: expression list, body: sourceElement list}
  | EXP_CALL of {member: expression, args: expression list}

and 

statement =
    ST_EXP of {exp: expression}
  | ST_BLOCK of {stmts: statement list}
  | ST_IF of {guard: expression, th: statement, el: statement}
  | ST_PRINT of {exp: expression}
  | ST_WHILE of {guard: expression, body: statement}
  | ST_RET of {exp: expression option}
  | ST_VAR of {var: expression list}
;

datatype program =
   PROGRAM of {elems: sourceElement list}
;