use "printAST.sml";

datatype value =
     Num_Value of int
   | String_Value of string
   | Bool_Value of bool
   | Undefined_Value
;

fun valueToString (Num_Value n) = 
   (if n < 0 then "-" ^ (Int.toString (~n)) else Int.toString n)
  | valueToString (String_Value s) =
   "\"" ^ (String.toString s) ^ "\""
  | valueToString (Bool_Value b) =
   Bool.toString b
  | valueToString Undefined_Value =
   "undefined"
;

fun typeString (Num_Value _) = "number"
  | typeString (Bool_Value _) = "boolean"
  | typeString (String_Value _) = "string"
  | typeString (Undefined_Value) = "undefined"
;

fun condTypeError found =
   error ("boolean guard required for 'cond' expression, found " ^
      (typeString found) ^ "\n")
;

fun unaryTypeError expected found oper =
   error ("unary operator '" ^
      (unaryOperatorString oper) ^ "' requires " ^
      (typeString expected) ^ ", found " ^ (typeString found) ^ "\n")
;

fun boolTypeError found oper =
   error ("operator '" ^ (binaryOperatorString oper) ^
      "' requires " ^ (typeString (Bool_Value true)) ^
      ", found " ^ (typeString found) ^ "\n")
;

fun binaryTypeError elft erht flft frht oper =
   error ("operator '" ^ (binaryOperatorString oper) ^ "' requires " ^
      (typeString elft) ^ " * " ^ (typeString erht) ^ ", found " ^
      (typeString flft) ^ " * " ^ (typeString frht) ^ "\n")
;

fun addTypeError flft frht oper =
   error ("operator '" ^ (binaryOperatorString oper) ^ "' requires " ^
      (typeString (Num_Value 0)) ^ " * " ^
      (typeString (Num_Value 0)) ^ " or " ^
      (typeString (String_Value "")) ^ " * " ^
      (typeString (String_Value "")) ^
      ", found " ^ (typeString flft) ^ " * " ^ (typeString frht) ^ "\n")
;

fun operatorFunc comp funcs oper =
   List.find (fn (opr, _) => comp (opr, oper)) funcs
;

fun applyArithOp _ fnc (Num_Value lft) (Num_Value rht) =
   Num_Value (fnc (lft, rht))
  | applyArithOp oper _ lft rht =
   binaryTypeError (Num_Value 0) (Num_Value 0) lft rht oper
;

fun applyDivOp _ fnc (Num_Value lft) (Num_Value rht) =
   if rht = 0
   then (error "divide by zero\n"; Undefined_Value)
   else Num_Value (fnc (lft, rht))
  | applyDivOp oper _ lft rht =
   binaryTypeError (Num_Value 0) (Num_Value 0) lft rht oper
;

fun applyRelOp _ fnc (Num_Value lft) (Num_Value rht) =
   Bool_Value (fnc (lft, rht))
  | applyRelOp oper _ lft rht =
   binaryTypeError (Num_Value 0) (Num_Value 0) lft rht oper
;

fun applyAddOp oper (Num_Value lft) (Num_Value rht) =
   Num_Value (lft + rht)
  | applyAddOp oper (String_Value lft) (String_Value rht) =
   String_Value (lft ^ rht)
  | applyAddOp oper lft rht =
   addTypeError lft rht oper
;

fun applyEqualityOp (Num_Value lft) (Num_Value rht) =
   Bool_Value (lft = rht)
  | applyEqualityOp (String_Value lft) (String_Value rht) =
   Bool_Value (lft = rht)
  | applyEqualityOp (Bool_Value lft) (Bool_Value rht) =
   Bool_Value (lft = rht)
  | applyEqualityOp Undefined_Value Undefined_Value =
   Bool_Value true
  | applyEqualityOp _ _ =
   Bool_Value false
;

fun applyInequalityOp x y =
   let
      val Bool_Value b = applyEqualityOp x y;
   in
      Bool_Value (not b)
   end
;

fun applyCommaOp _ rht = rht;

fun applyEagerBoolOp _ fnc (Bool_Value lft) (Bool_Value rht) =
   Bool_Value (fnc (lft, rht))
  | applyEagerBoolOp oper _ lft rht =
   binaryTypeError (Bool_Value true) (Bool_Value true) lft rht oper
;

fun applyEagerAndOp oper lft rht =
   applyEagerBoolOp oper (fn (a, b) => a andalso b) lft rht
;

fun applyEagerOrOp oper lft rht =
   applyEagerBoolOp oper (fn (a, b) => a orelse b) lft rht
;

val binaryFuncs = [
   (BOP_PLUS, applyAddOp BOP_PLUS),
   (BOP_MINUS, applyArithOp BOP_MINUS (op -)),
   (BOP_TIMES, applyArithOp BOP_TIMES (op * )),
   (BOP_DIVIDE, applyDivOp BOP_DIVIDE (op div)),
   (BOP_MOD, applyDivOp BOP_MOD (op mod)),
   (BOP_EQ, applyEqualityOp),
   (BOP_NE, applyInequalityOp),
   (BOP_LT, applyRelOp BOP_LT (op <)),
   (BOP_GT, applyRelOp BOP_GT (op >)),
   (BOP_LE, applyRelOp BOP_LE (op <=)),
   (BOP_GE, applyRelOp BOP_GE (op >=)),
   (BOP_AND, applyEagerAndOp BOP_AND),
   (BOP_OR, applyEagerOrOp BOP_OR),
   (BOP_COMMA, applyCommaOp)
];

val binaryOperatorFunc =
   operatorFunc ((op =) : binaryOperator * binaryOperator -> bool) binaryFuncs
;

fun applyNotOp _ (Bool_Value b) =
   Bool_Value (not b)
  | applyNotOp oper opnd =
   unaryTypeError (Bool_Value true) opnd oper
;

fun applyMinusOp _ (Num_Value n) =
   Num_Value (~n)
  | applyMinusOp oper opnd =
   unaryTypeError (Num_Value 0) opnd oper
;

fun applyTypeofOp v = String_Value (typeString v);

val unaryFuncs = [
   (UOP_NOT, applyNotOp UOP_NOT),
   (UOP_TYPEOF, applyTypeofOp),
   (UOP_MINUS, applyMinusOp UOP_MINUS)
];

val unaryOperatorFunc =
   operatorFunc ((op =) : unaryOperator * unaryOperator -> bool) unaryFuncs
;

fun verifyBoolValue (v as Bool_Value b) oper =
   v
  | verifyBoolValue v oper =
   binaryTypeError (Bool_Value true) (Bool_Value true)
      (Bool_Value true) v oper
;

fun evalBinary BOP_AND lft rht =
   (case evalExpression lft of
       Bool_Value true => verifyBoolValue (evalExpression rht) BOP_AND
    |  Bool_Value false => Bool_Value false
    |  v => boolTypeError v BOP_AND
   )
  | evalBinary BOP_OR lft rht =
   (case evalExpression lft of
       Bool_Value true => Bool_Value true
    |  Bool_Value false => verifyBoolValue (evalExpression rht) BOP_OR
    |  v => boolTypeError v BOP_OR
   )
  | evalBinary oper lft rht =
   case (binaryOperatorFunc oper) of
      SOME (_, func) =>
         func (evalExpression lft) (evalExpression rht)
   |  NONE =>
         error ("operator '" ^ (binaryOperatorString oper) ^ "' not found\n")
and evalUnary oper opnd =
   case (unaryOperatorFunc oper) of
      SOME (_, func) => func (evalExpression opnd)
   |  NONE =>
         error ("operator '" ^ (unaryOperatorString oper) ^ "' not found\n")
and evalExpression (EXP_NUM n) =
   Num_Value n
  | evalExpression (EXP_STRING s) =
   String_Value s
  | evalExpression EXP_TRUE =
   Bool_Value true
  | evalExpression EXP_FALSE =
   Bool_Value false
  | evalExpression EXP_UNDEFINED =
   Undefined_Value
  | evalExpression (EXP_BINARY {opr, lft, rht}) =
   evalBinary opr lft rht
  | evalExpression (EXP_UNARY {opr, opnd}) =
   evalUnary opr opnd
  | evalExpression (EXP_COND {guard, thenExp, elseExp}) =
   (case evalExpression guard of
       Bool_Value true => evalExpression thenExp
    |  Bool_Value false => evalExpression elseExp
    |  v => condTypeError v
   )
;

fun evalStatement (ST_EXP {exp}) =
   let
      val res = evalExpression exp;
   in
      (out (expressionString exp);
       out " ==> ";
       out (valueToString res);
       out "\n"
      )
   end
;

fun evalSourceElement (STMT {stmt}) =
   evalStatement stmt
;

fun evalSourceElements [] = ()
  | evalSourceElements (el::els) =
   (evalSourceElement el; evalSourceElements els)
;

fun evalProgram (PROGRAM {elems}) =
   evalSourceElements elems
;

fun interpret file =
   (evalProgram (parse file); ())
;
