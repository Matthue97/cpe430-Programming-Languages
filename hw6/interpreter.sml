use "printAST.sml";
use "map.sml";

datatype value =
     Num_Value of int
   | String_Value of string
   | Bool_Value of bool
   | Undefined_Value
   | Function_Value of {params: string list, body: sourceElement list,
      closure: (string, value) HashTable.hash_table list, this: unit ref}
;

exception BadEnv;

fun valueToString (Num_Value n) = 
   (if n < 0 then "-" ^ (Int.toString (~n)) else Int.toString n)
  | valueToString (String_Value s) = s
  | valueToString (Bool_Value b) = Bool.toString b
  | valueToString Undefined_Value = "undefined"
;

fun typeString (Num_Value _) = "number"
  | typeString (Bool_Value _) = "boolean"
  | typeString (String_Value _) = "string"
  | typeString (Undefined_Value) = "undefined"
;
(* PART4 *)
fun addGlobal scope x result =
  (case (tl scope) of
     [] => (insert (hd scope) x result; result)
    | _ => addGlobal (tl scope) x result)

fun envChain {chain, retVal} = chain;

fun lookupEnvH [] id = Undefined_Value
  | lookupEnvH (env::envs) id =
   if contains env id
   then lookup env id
   else lookupEnvH envs id
;
fun lookupEnv env id = lookupEnvH (envChain env) id;

fun insertEnvH [] id v = raise BadEnv
  | insertEnvH [global] id v = insert global id v
  | insertEnvH (env::envs) id v =
   if contains env id
   then insert env id v
   else insertEnvH envs id v
;

fun insertEnv env id v = (insertEnvH (envChain env) id v; env);
fun envRetVal {chain, retVal} = retVal;
fun createEnv chain retVal = {chain=chain, retVal=retVal};
fun growEnvironment env =
   createEnv ((new_map ())::(envChain env)) (envRetVal env)
;

fun createClosureValue params body env =
   Function_Value {params=params, body=body, closure=env, this=ref ()}
;

fun insertCurrent env id v = (insert (hd (envChain env)) id v; env);

(* END PART4 *)
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
      (typeString (String_Value "")) ^ ", found " ^
      (typeString flft) ^ " * " ^ (typeString frht) ^ "\n")
;

fun ifTypeError found =
   error ("boolean guard required for 'if' statement, found " ^
      (typeString found) ^ "\n")
;

fun whileTypeError found =
   error ("boolean guard required for 'while' statement, found " ^
      (typeString found) ^ "\n")
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
  | applyEqualityOp (Function_Value lft) (Function_Value rht) =
   Bool_Value ((#this lft) = (#this rht))
  | applyEqualityOp Undefined_Value Undefined_Value =
   Bool_Value true
  | applyEqualityOp _ _ =
   Bool_Value false
;

fun applyInequalityOp x y =
   let val Bool_Value b = applyEqualityOp x y;
   in Bool_Value (not b)
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

fun evalExpressions (x::xs) scope =
   ((evalExpression x scope)::(evalExpressions xs scope))
  | evalExpressions [] scope = []
and evalBinary BOP_AND lft rht scope =
   (case evalExpression lft scope of
       Bool_Value true => verifyBoolValue (evalExpression rht scope) BOP_AND
    |  Bool_Value false => Bool_Value false
    |  v => boolTypeError v BOP_AND
   )
  | evalBinary BOP_OR lft rht scope =
   (case evalExpression lft scope of
       Bool_Value true => Bool_Value true
    |  Bool_Value false => verifyBoolValue (evalExpression rht scope) BOP_OR
    |  v => boolTypeError v BOP_OR
   )
  | evalBinary oper lft rht scope =
   case (binaryOperatorFunc oper) of
      SOME (_, func) =>
         func (evalExpression lft scope) (evalExpression rht scope)
   |  NONE =>
         error ("operator '" ^ (binaryOperatorString oper) ^ "' not found\n")
and evalUnary oper opnd scope =
   case (unaryOperatorFunc oper) of
      SOME (_, func) => func (evalExpression opnd scope)
   |  NONE =>
         error ("operator '" ^ (unaryOperatorString oper) ^ "' not found\n")
and evalExpression (EXP_ID id) scope =
    (lookupEnv scope id)
  | evalExpression (EXP_NUM n) scope = Num_Value n
  | evalExpression (EXP_STRING s) scope = String_Value s
  | evalExpression EXP_TRUE scope = Bool_Value true
  | evalExpression EXP_FALSE scope = Bool_Value false
  | evalExpression EXP_UNDEFINED scope = Undefined_Value
  | evalExpression (EXP_BINARY {opr, lft, rht}) scope =
   evalBinary opr lft rht scope
  | evalExpression (EXP_UNARY {opr, opnd}) scope =
   evalUnary opr opnd scope
  | evalExpression (EXP_COND {guard, thenExp, elseExp}) scope =
   (case evalExpression guard scope of
       Bool_Value true => evalExpression thenExp scope
    |  Bool_Value false => evalExpression elseExp scope
    |  v => condTypeError v
   )
  | evalExpression (EXP_ASSIGN {lhs, rhs}) scope =
   let
      val rhs_result = evalExpression rhs scope;
   in
      case lhs of
        EXP_ID s => (insertEnv  scope s rhs_result; rhs_result)
      |  _ => error "unexpected target of assignment\n"
   end
  | evalExpression (EXP_FUNC {name, params, body}) env =
   let
      val newenv = growEnvironment env;
      val func =
         createClosureValue params body (envChain newenv);
      val _ = insertCurrent newenv name func;
   in
      func
   end
  | evalExpression (EXP_CALL {func, args}) scope =
   (case evalExpression func scope of
       Function_Value {params, body, closure, this} =>
          evalFuncCall func (evalExpressions args scope) ((new_map())::closure)
    |  v => error ("attempt to invoke '" ^ (typeString v) ^
          "' value as a function\n")
   )
and evalFuncCall (func as EXP_FUNC {name, params, body}) args scope =
   (
    if isSome name
    then bindParam (valOf name) (evalExpression func scope) scope
    else scope
    ;
    bindParams params args scope;
    case contains (evalSourceElements body scope) "return" of
       NONE => Undefined_Value
    |  SOME x => x
   )
  | evalFuncCall _ _ _ = error "unimplemented"
and bindParam param arg scope =
   (case param of
       EXP_ID p => insert scope p arg
    |  EXP_VAR {bind, expreval} => bindParam bind arg scope
    |  _ => error "unimplemented"
   )
and bindParams (param::params) (arg::args) scope =
   bindParams params args (bindParam param arg scope)
  | bindParams (param::params) [] scope =
   bindParams params [] (bindParam param Undefined_Value scope)
  | bindParams [] _ scope = scope

and evalStatement _ (scope as {chain, retVal=SOME _}) = scope |
  evalStatement (ST_EXP {exp}) scope =
   (evalExpression exp scope; scope)
  | evalStatement (ST_BLOCK {stmts}) scope =
   evalBlockStatement stmts scope
  | evalStatement (ST_IF {guard, th, el}) scope =
   (case evalExpression guard scope of
       (Bool_Value true) => evalStatement th scope
    |  (Bool_Value false) => evalStatement el scope
    |  v => ifTypeError v
   )
  | evalStatement (ST_PRINT {exp}) scope =
   (TextIO.output (TextIO.stdOut, valueToString (evalExpression exp scope));
    scope)
  | evalStatement (ST_WHILE {guard, body}) scope =
   (if isSome (contains scope "return")
    then scope
    else case evalExpression guard scope of
            Bool_Value true => evalStatement (ST_WHILE {guard=guard, body=body})
             (evalStatement body scope)
         |  Bool_Value false => scope
         |  v => whileTypeError v
   )
  | evalStatement (ST_VAR {vars}) scope = evalVarStatement vars scope
  | evalStatement (ST_RETURN {exp}) scope = evalReturnStatement exp scope
and evalBlockStatement [] scope = scope
  | evalBlockStatement (stmt::stmts) scope =
   evalBlockStatement stmts (evalStatement stmt scope)
and evalVarStatement [] scope = scope
  | evalVarStatement (var::vars) scope =
   (evalExpression var scope; evalVarStatement vars scope)
and evalReturnStatement exp (scope as (map::(xs::xss))) =
   (
      insert scope "return" (
         if isSome exp
         then (evalExpression (valOf exp) scope)
         else Undefined_Value
      )
      ;
      scope
   )
  | evalReturnStatement exp (map::xs) =
   error "return statements are only valid inside functions"
  | evalReturnStatement exp [] = error "unimplemented"

and evalSourceElement (STMT {stmt}) scope = evalStatement stmt scope
  | evalSourceElement (FUNCT_DECLAR {funct}) scope =
   (case funct of
       EXP_FUNCT {bind, params, body} =>
          bindParam (valOf bind) (evalExpression funct scope) scope
    |  _ => error "unimplemented"
   )
and evalSourceElements elements scope =
   let
      fun evalSrcElH [] scope = scope
        | evalSrcElH (elements as (el::els)) scope =
           if isSome (contains scope "return")
           then scope
           else evalSrcElH els (evalSourceElement el scope)
      ;
   in
      evalSrcElH elements (defineSourceElements elements scope)
   end

and defineSourceElement (STMT {stmt}) scope =
   (case stmt of
       ST_BLOCK {stmts} =>
          defineSourceElements (createSourceElements stmts) scope
    |  ST_IF {guard, th, el} => (
          defineSourceElement (STMT {stmt = th}) scope;
          defineSourceElement (STMT {stmt = el}) scope
       )
    |  ST_WHILE {guard, body} => defineSourceElement (STMT {stmt = body}) scope
    |  ST_VAR {vars} => bindParams vars [] scope
    |  _ => scope
   )
  | defineSourceElement (FUNCT_DECLAR {funct}) scope =
   (case funct of
       EXP_FUNCT {bind, params, body} =>
          bindParams [(valOf bind)] [(evalExpression funct scope)] scope
    |  _ => scope
   )
and defineSourceElements [] scope = scope
  | defineSourceElements (el::els) scope =
    defineSourceElements els (defineSourceElement el scope)
and createSourceElements (st::sts) =
   ((STMT {stmt = st})::(createSourceElements sts))
  | createSourceElements [] = []
;

fun evalProgram (PROGRAM {elems}) scope =
   evalSourceElements elems scope
;

fun interpret file =
   (evalProgram (parse file) [(new_map ())]; ())
;