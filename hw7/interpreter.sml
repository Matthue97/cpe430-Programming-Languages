use "printAST.sml";
use "map.sml";

exception MalformedEnvironment;

datatype value =
     Num_Value of int
   | String_Value of string
   | Bool_Value of bool
   | Undefined_Value
   | Object of {table: (string, value) HashTable.hash_table, rf: unit ref}
   | Closure_Value of {params: string list, body: sourceElement list,
      env: (string, value) HashTable.hash_table list, rf: unit ref, props:(string, value) HashTable.hash_table}
;
val global_object = (Object {table = (new_map ()), rf=ref ()})
val object_prototype = (Object {table = (new_map ()), rf=ref ()})
val function_prototype = (Object {table = (new_map ()), rf=ref ()})

fun getGlobalTbl (Object {table,rf}) = table
  | getGlobalTbl v = raise MalformedEnvironment
;

fun expressionString (EXP_ID s) = s
  | expressionString (EXP_NUM n) =
    if n < 0 then "-" ^ (Int.toString (~n)) else Int.toString n
  | expressionString (EXP_STRING s) = "\"" ^ (String.toString s) ^ "\""
  | expressionString EXP_TRUE = "true"
  | expressionString EXP_FALSE = "false"
  | expressionString EXP_UNDEFINED = "undefined"
  | expressionString EXP_THIS = "this"
  | expressionString (EXP_BINARY {opr, lft, rht}) =
   "(" ^
       (expressionString lft) ^
       " " ^ (binaryOperatorString opr) ^ " " ^
       (expressionString rht) ^
   ")"
  | expressionString (EXP_UNARY {opr, opnd}) =
   "(" ^
     (unaryOperatorString opr) ^ (expressionString opnd) ^
   ")"
  | expressionString (EXP_COND {guard, thenExp, elseExp}) =
   "(" ^
       (expressionString guard) ^
       " ? " ^
       (expressionString thenExp) ^
       " : " ^
       (expressionString elseExp) ^
   ")"
  | expressionString (EXP_ASSIGN {lhs, rhs}) =
   "(" ^
       (expressionString lhs) ^ " = " ^ (expressionString rhs) ^
   ")"
  | expressionString (EXP_CALL {func, args}) =
   (expressionString func) ^ "(" ^ (argumentsString args) ^ ")"
  | expressionString (EXP_FUNC {name, params, body}) =
   "(" ^
      funcString (SOME name) params body ^
   ")"
  | expressionString (EXP_ANON {params, body}) =
   "(" ^
      funcString (NONE) params body ^
   ")"
  | expressionString (EXP_DOT {lft, id}) =
   "(" ^ (expressionString lft) ^ "." ^ id ^ ")"
  | expressionString (EXP_NEW {ctorExp, args}) =
   "(new " ^
       (expressionString ctorExp) ^
       "(" ^ (argumentsString args) ^ ")" ^
   ")"
  | expressionString (EXP_OBJ {props}) =
   "{\n" ^ (propertiesString props) ^ "}"
;

fun createClosureValue params body env =
   let
       val table = (new_map ())
       val construct = (new_map ())
    in
      (
        insert construct "proto_chain" object_prototype;
        let
          val constrObj = (Object{table = construct, rf=ref ()})
          val clos = Closure_Value {params=params, body=body, env=env, rf=ref (), props = table}
        in
          (insert construct "constructor" clos; insert table "prototype" constrObj; clos)
        end
      )
   end
  
;

fun valueToString (Num_Value n) = 
   (if n < 0 then "-" ^ (Int.toString (~n)) else Int.toString n)
  | valueToString (String_Value s) = s
  | valueToString (Bool_Value b) = Bool.toString b
  | valueToString Undefined_Value = "undefined"
  | valueToString (Closure_Value _) = "function"
  | valueToString (Object _) = "object"
;

fun typeString (Num_Value _) = "number"
  | typeString (Bool_Value _) = "boolean"
  | typeString (String_Value _) = "string"
  | typeString (Undefined_Value) = "undefined"
  | typeString (Closure_Value _) = "function"
  | typeString (Object _) = "object"
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

fun ifTypeError found =
   error ("boolean guard required for 'if' statement, found " ^
      (typeString found) ^ "\n")
;

fun whileTypeError found =
   error ("boolean guard required for 'while' statement, found " ^
      (typeString found) ^ "\n")
;

fun envChain {chain, retVal} = chain;
fun envRetVal {chain, retVal} = retVal;
fun createEnv chain retVal = {chain=chain, retVal=retVal};

fun atTopLevel {chain=[tle], retVal} = true
  | atTopLevel {chain, retVal} = false
;

fun insertCurrent env id v = (insert (hd (envChain env)) id v; env);
fun declareCurrent env id =
   if contains (hd (envChain env)) id
   then env
   else (insert (hd (envChain env)) id Undefined_Value; env)
;
fun insertEnvH [] id v = raise MalformedEnvironment
  | insertEnvH [global] id v = insert global id v
  | insertEnvH (env::envs) id v =
   if contains env id
   then insert env id v
   else insertEnvH envs id v
;
fun insertEnv env id v = (insertEnvH (envChain env) id v; env);
fun lookupEnvH [] id = raise UndefinedIdentifier
  | lookupEnvH (env::envs) id =
   if contains env id
   then lookup env id
   else lookupEnvH envs id
;
fun lookupEnv env id = lookupEnvH (envChain env) id;
fun growEnvironment env =
   createEnv ((new_map ())::(envChain env)) (envRetVal env)
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
  | applyEqualityOp (Closure_Value lft) (Closure_Value rht) =
   Bool_Value (#rf lft = #rf rht)
  | applyEqualityOp (Object lft) (Object rht) =
   Bool_Value (#rf lft = #rf rht)
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

fun splitSourceElementsH [] stmts funcs = (rev stmts, rev funcs)
  | splitSourceElementsH ((STMT {stmt})::elems) stmts funcs =
   splitSourceElementsH elems (stmt::stmts) funcs
  | splitSourceElementsH ((FUNC_DECL func)::elems) stmts funcs =
   splitSourceElementsH elems stmts (func::funcs)
;

fun splitSourceElements elems = splitSourceElementsH elems [] [];

fun declareFunctions [] env = env
  | declareFunctions ({name, params, body}::funcs) env =
   declareFunctions funcs
      (insertCurrent env name (createClosureValue params body (envChain env)))
;

fun declareVars [] env = env
  | declareVars ((DECL_ID d)::decls) env =
   declareVars decls (declareCurrent env (#id d))
  | declareVars ((DECL_INIT d)::decls) env =
   declareVars decls (declareCurrent env (#id d))
;

fun declareVariables [] env = env
  | declareVariables (stmt::stmts) env =
   declareVariables stmts (declareStmtVariables stmt env)
and declareStmtVariables (ST_VAR v) env =
   declareVars (#decls v) env
  | declareStmtVariables (ST_BLOCK b) env =
   declareVariables (#stmts b) env
  | declareStmtVariables (ST_IF i) env =
   declareStmtVariables (#el i) (declareStmtVariables (#th i) env)
  | declareStmtVariables (ST_WHILE w) env =
   declareStmtVariables (#body w) env
  | declareStmtVariables _ env = env
;

fun bindParameters [] args env = env
  | bindParameters (p::ps) [] env =
   bindParameters ps [] (insertCurrent env p Undefined_Value)
  | bindParameters (p::ps) (arg::args) env =
   bindParameters ps args (insertCurrent env p arg)
;

fun evalBinary BOP_AND lft rht env =
   (case evalExpression lft env of
       Bool_Value true => verifyBoolValue (evalExpression rht env) BOP_AND
    |  Bool_Value false => Bool_Value false
    |  v => boolTypeError v BOP_AND
   )
  | evalBinary BOP_OR lft rht env =
   (case evalExpression lft env of
       Bool_Value true => Bool_Value true
    |  Bool_Value false => verifyBoolValue (evalExpression rht env) BOP_OR
    |  v => boolTypeError v BOP_OR
   )
  | evalBinary oper lft rht env =
   case (binaryOperatorFunc oper) of
      SOME (_, func) =>
         func (evalExpression lft env) (evalExpression rht env)
   |  NONE =>
         error ("operator '" ^ (binaryOperatorString oper) ^ "' not found\n")
and evalUnary oper opnd env =
   case (unaryOperatorFunc oper) of
      SOME (_, func) => func (evalExpression opnd env)
   |  NONE =>
         error ("operator '" ^ (unaryOperatorString oper) ^ "' not found\n")
and evalExpression (EXP_ID s) env =
   (lookupEnv env s handle UndefinedIdentifier =>
      error ("variable '" ^ s ^ "' not found\n"))
  | evalExpression (EXP_NUM n) env =
   Num_Value n
  | evalExpression (EXP_STRING s) env =
   String_Value s
  | evalExpression EXP_TRUE env =
   Bool_Value true
  | evalExpression EXP_FALSE env =
   Bool_Value false
  | evalExpression EXP_UNDEFINED env =
   Undefined_Value
  | evalExpression EXP_THIS env = 
    (if(atTopLevel env) then global_object else lookupEnv env "this" handle UndefinedIdentifier => error ("variable '" ^ "this" ^ "' not found\n"))
  | evalExpression (EXP_BINARY {opr, lft, rht}) env =
   evalBinary opr lft rht env
  | evalExpression (EXP_UNARY {opr, opnd}) env =
   evalUnary opr opnd env
  | evalExpression (EXP_COND {guard, thenExp, elseExp}) env =
   (case evalExpression guard env of
       Bool_Value true => evalExpression thenExp env
    |  Bool_Value false => evalExpression elseExp env
    |  v => condTypeError v
   )
  | evalExpression (EXP_ASSIGN {lhs, rhs}) env =
   let
      val rhs = evalExpression rhs env;
   in
      (case lhs of
          EXP_ID s => (insertEnv env s rhs; Bool_Value true)
       |  (EXP_DOT {lft,id}) =>
              let
                  val obj = evalExpression lft env
              in
                  (*(out ((expressionString lft)^"\n"));*)
                  (case obj of
                    (Object{table,rf}) => (insert table id rhs; Bool_Value true)
                    | (Closure_Value {params, body, env, rf , props}) =>
                    (insert props id rhs; Bool_Value true)
                    | _ => error ("field reference '.' requires object, found " ^ (valueToString obj) ^ "\n")
                  )
              end
       |  _ => error "unexpected target of assignment\n"
       ;
       rhs
      )
   end
  | evalExpression (EXP_CALL {func, args}) env =
   (case func of 
      (EXP_DOT{lft, id}) =>
        let
            val obj = evalExpression lft env
        in
            (case obj of
              (Object{table, rf}) => 
                let
                    val table_val = lookup table id handle UndefinedIdentifier => (traverseObjects (lookup table "proto_chain") id)
                in
                    (case table_val of
                        Closure_Value closure =>
                           let
                              val arg_vals =
                                 List.map (fn exp => evalExpression exp env) args;
                              val new_env = bindParameters (#params closure) arg_vals (growEnvironment (createEnv (#env closure) NONE));
                            in
                                insertCurrent new_env "this" obj;
                                let
                                  val (stmts, funcs) = splitSourceElements (#body closure);
                                  val closure_env = evalStatements stmts (declareVariables stmts (declareFunctions funcs new_env));
                                in
                                  case envRetVal closure_env of
                                     SOME v => v
                                  |  NONE => Undefined_Value
                                end
                           end
                      | e => error ("attempt to invoke '"  ^ (expressionString lft) ^ "." ^ id ^ "' as a function\n")
                     )
                end
              | _ => error ("field reference '.' requires object, found " ^ (typeString obj) ^ "\n")
            )
        end
     | _ => (case evalExpression func env of
        Closure_Value closure =>
           let
              (* evaluate arguments in current environment *)
              val arg_vals =
                 List.map (fn exp => evalExpression exp env) args;
              (* bind parameters in new environment *)
              val newenv = bindParameters
                 (#params closure)
                 arg_vals
                 (growEnvironment (createEnv (#env closure) NONE));
              val buff = (insertEnv newenv "this" global_object)

              val (stmts, funcs) =
                 splitSourceElements (#body closure);
              (* declare functions, declare variables, evaluate body *)
              val closure_env = evalStatements stmts
                 (declareVariables stmts (declareFunctions funcs newenv));
           in
              case envRetVal closure_env of
                 SOME v => v
              |  NONE => Undefined_Value
           end
      | e => error ("attempt to invoke '" ^ typeString(e) ^
        "' value as a function\n")
     )
    )
  | evalExpression (EXP_DOT {lft,id}) env = 
    let
        val obj = (evalExpression lft env)
    in
        (case obj of
          (Object{table,rf}) => (lookup table id handle UndefinedIdentifier => traverseObjects (lookup table "proto_chain") id)
          | (Closure_Value {params, body, env, rf , props}) =>
            let
                val proto = lookup props id
            in
                proto
            end
          | _ => error ("field reference '.' requires object, found " ^ (typeString obj) ^ "\n")
        )
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
  | evalExpression (EXP_ANON {params, body}) env = createClosureValue params body (envChain env)
  | evalExpression (EXP_NEW{ctorExp, args}) env = 
      (
        case evalExpression ctorExp env of
          Closure_Value closure =>
             let
                val objTable = (new_map ())
                val prop_val = lookup (#props closure) "prototype";
                val proto_val = (
                  case prop_val of
                      (Object _) => prop_val
                    | (Closure_Value _) => prop_val
                    | _ => object_prototype
                )
              in
                (
                  insert objTable "proto_chain" proto_val;
                  let
                    val obj = (Object{table = objTable, rf=ref ()});
                    val arg_vals = List.map (fn exp => evalExpression exp env) args;
                    val newenv = bindParameters
                       (#params closure)
                       arg_vals
                       (growEnvironment (createEnv (#env closure) NONE));
                  in
                    (
                      insertCurrent newenv "this" obj;
                      let
                        val (stmts, funcs) = splitSourceElements (#body closure);
                        val closure_env = evalStatements stmts (declareVariables stmts (declareFunctions funcs newenv));
                      in
                        case envRetVal closure_env of
                           SOME v => 
                            (case v of
                              (Object _) => v
                              | (Closure_Value _) => v
                              | _ => obj 
                            )
                        |  NONE => obj
                      end
                    )
                  end
                )
             end
        | e => error ("new may only be applied to a function, found " ^ typeString(e) ^ "\n")
     )
  | evalExpression (EXP_OBJ {props}) env =
  let
      val objTable = (new_map ())
      val obj = evalObject props objTable env
  in
      (
        insert objTable "proto_chain" object_prototype;
        let
          val obj = evalObject props objTable env
        in
          (Object {table = obj, rf=ref ()})
        end
      )
  end
and traverseObjects (Object{table,rf}) id =
    (lookup table id handle UndefinedIdentifier => traverseObjects (lookup table "proto_chain") id)
  | traverseObjects x id = Undefined_Value
and getProto table =
  let
      val x = lookup table "prototype";
  in
      case x of
      (Object _) => x
      | (Closure_Value _) => x
      | _ => object_prototype
  end

and evalVariables [] env = env
  | evalVariables ((DECL_ID _)::decls) env =
   evalVariables decls env
  | evalVariables ((DECL_INIT {id, src})::decls) env =
   evalVariables decls
      (insertCurrent env id (evalExpression src env))

and evalStatement _ (env as {chain, retVal=SOME _}) = env
  | evalStatement (ST_EXP {exp}) env =
   evalExpStatement exp env
  | evalStatement (ST_BLOCK {stmts}) env =
   evalStatements stmts env
  | evalStatement (ST_IF {guard, th, el}) env =
   evalIfStatement guard th el env
  | evalStatement (ST_PRINT {exp}) env =
   evalPrintStatement exp env
  | evalStatement (ST_WHILE {guard, body}) env =
   evalWhileStatement guard body env
  | evalStatement (ST_VAR {decls}) env =
   evalVariables decls env
  | evalStatement (ST_RETURN {exp}) env =
   if not (atTopLevel env)
   then createEnv (envChain env) (SOME (evalExpression exp env))
   else error ("return statements are only valid inside functions\n")
and evalExpStatement exp env =
   (evalExpression exp env; env)
and evalIfStatement guard th el env =
   case evalExpression guard env of
      Bool_Value true => evalStatement th env
   |  Bool_Value false => evalStatement el env
   |  v => ifTypeError v
and evalPrintStatement exp env =
   (TextIO.output (TextIO.stdOut, valueToString (evalExpression exp env)); env)
and evalWhileStatement guard body env =
   case evalExpression guard env of
      Bool_Value true =>
         evalStatement
            (ST_WHILE {guard=guard, body=body}) (evalStatement body env)
   |  Bool_Value false => env
   |  v => whileTypeError v

and evalStatements [] env = env
  | evalStatements (stmt::stmts) env =
   evalStatements stmts (evalStatement stmt env)

and evalObject [] objTable env = objTable
  | evalObject (prop::props) objTable env = 
    (
      case prop of
        ({id,exp}) => (
          insert objTable id (evalExpression exp env);
          evalObject props objTable env
        )
    )
;

fun createEnvironment () = createEnv [(getGlobalTbl global_object)] NONE;

fun evalProgram (PROGRAM {elems}) =
   let
      val (stmts, funcs) = splitSourceElements elems
   in
    (
      (* Initialize the global object, object origin, and function origin. *)
      insert (getGlobalTbl global_object) "proto_chain" object_prototype;
      insert (getGlobalTbl object_prototype) "proto_chain" Undefined_Value;
      insert (getGlobalTbl function_prototype) "proto_chain" object_prototype;

      evalStatements stmts
         (declareVariables stmts
            (declareFunctions funcs (createEnvironment ())))
    )
   end
;

fun interpret file =
   (evalProgram (parse file); ())
;
