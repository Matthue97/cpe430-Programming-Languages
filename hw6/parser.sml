(* CPE430 Assignment 6 part 1-3 Kevin Yang*)
open TextIO;

use "ast.sml";
use "tokenizer.sml";

fun err_expect want got =
   error ("expected '" ^ want ^ "', found '" ^ got ^"'\n")
;

fun match_id fstr (TK_ID x) = (x, nextToken fstr)
  | match_id fstr tk = err_expect "identifier" (tkString tk)
;

fun match_num fstr (TK_NUM n) = (n, nextToken fstr)
  | match_num fstr tk = err_expect "number" (tkString tk)
;

fun match_string fstr (TK_STRING s) = (s, nextToken fstr)
  | match_string fstr tk = err_expect "string" (tkString tk)
;

fun match_tk fstr tk expected =
   if tk = expected
   then nextToken fstr
   else err_expect (tkString expected) (tkString tk)
;

fun match_eof fstr TK_EOF = TK_EOF
  | match_eof fstr tk = err_expect (tkString TK_EOF) (tkString tk)
;

fun findPair s xs = List.find (fn (st, _) => st = s) xs;
fun inOps tk ops = isSome (findPair tk ops);

val eqOps = [(TK_EQ, BOP_EQ), (TK_NE, BOP_NE)];
val relOps =
   [(TK_LT, BOP_LT), (TK_GT, BOP_GT), (TK_LE, BOP_LE), (TK_GE, BOP_GE)];
val addOps = [(TK_PLUS, BOP_PLUS), (TK_MINUS, BOP_MINUS)];
val multOps =
   [(TK_TIMES, BOP_TIMES), (TK_DIVIDE, BOP_DIVIDE), (TK_MOD, BOP_MOD)];
val unaryOps =
   [(TK_NOT, UOP_NOT), (TK_TYPEOF, UOP_TYPEOF), (TK_MINUS, UOP_MINUS)];
val andOps = [(TK_AND, BOP_AND)];
val orOps = [(TK_OR, BOP_OR)];
val commaOps = [(TK_COMMA, BOP_COMMA)];

fun opErrorMsgList [] strs= []
  | opErrorMsgList ((x, _)::[]) strs = (tkString x) :: strs
  | opErrorMsgList ((x, _)::(y, _)::[]) strs =
       (tkString y)::" or "::(tkString x) :: strs
  | opErrorMsgList ((x, _)::(y, _)::(z, _)::[]) strs =
      (tkString z)::", or "::(tkString y)::", "::(tkString x)::strs
  | opErrorMsgList ((x, _)::xs) strs =
      opErrorMsgList xs (", "::(tkString x)::strs)
;
fun opErrorMsg ops = foldl (op ^) "" (opErrorMsgList ops []);

fun isEqOp tk = inOps tk eqOps;
fun isRelOp tk = inOps tk relOps;
fun isAddOp tk = inOps tk addOps;
fun isMultOp tk = inOps tk multOps;
fun isUnaryOp tk = inOps tk unaryOps;
fun isAndOp tk = inOps tk andOps;
fun isOrOp tk = inOps tk orOps;
fun isCommaOp tk = inOps tk commaOps;

fun isIdentifier (TK_ID _) = true
  | isIdentifier _ = false
;

fun isExpression TK_LPAREN = true
  | isExpression (TK_ID _) = true
  | isExpression (TK_NUM _) = true
  | isExpression (TK_STRING _) = true
  | isExpression TK_TRUE = true
  | isExpression TK_FALSE = true
  | isExpression TK_UNDEFINED = true
  | isExpression TK_NOT = true
  | isExpression TK_TYPEOF = true
  | isExpression TK_MINUS = true
  | isExpression TK_FUNCTION = true
  | isExpression _ = false;

fun isValidLHS (EXP_ID _) = true
  | isValidLHS _ = false
;

fun isStatement tk =
   not (tk = TK_FUNCTION) andalso
   (tk = TK_LBRACE orelse tk = TK_IF orelse tk = TK_PRINT orelse
   tk = TK_WHILE orelse tk = TK_VAR orelse tk = TK_RETURN orelse
   isExpression tk)
;

fun isSourceElement tk =
   isStatement tk orelse tk = TK_FUNCTION
;

fun parseRepetition fstr tk pred parse_single =
  let fun parseRepetitionHelper fstr tk pred parse_single xs =
      if pred tk
      then
         let val (x, tk1) = parse_single fstr tk;
         in parseRepetitionHelper fstr tk1 pred parse_single (x::xs)
         end
      else
         (rev xs, tk)
   ;
   in parseRepetitionHelper fstr tk pred parse_single []
   end
;

fun list_err_msg list_type = 
  case list_type of
  "params" => (fn _ => error "") |
  "args" => (fn _ => error "") |
  "vars" => (fn x => err_expect "identifier" (tkString x)) |
  _ => (fn _ => error "")
;

fun list_empty_permission list_type = 
  case list_type of
  "params" => true |
  "args" => true |
  "vars" => false |
  _ => false

fun parseListEmptyHelper inStream token parse_expr is_check error_msg_func curList =
        if is_check token then (
          let 
            val (list_item, tk1) = parse_expr inStream token
          in
            if tk1 = TK_COMMA then 
              (parseListEmptyHelper inStream (nextToken inStream) parse_expr is_check error_msg_func (list_item::curList))
            else 
              (rev (list_item::curList), tk1)
          end
        )
        else 
          (rev curList, token)
;

fun parseListHelper inStream token parse_expr is_check error_msg_func curList =
  if is_check token then (
    let 
      val (list_item, tk1) = (parse_expr inStream token)
    in
      if tk1 = TK_COMMA then 
        (parseListHelper inStream (nextToken inStream) parse_expr is_check error_msg_func (list_item::curList))
      else 
        (rev (list_item::curList), tk1)
    end
  )
  else 
    (error_msg_func token)
;

(* expression parsing functions *)
fun parseOp fstr tk ops =
   case findPair tk ops of
      SOME (tk1, opr) => (opr, match_tk fstr tk tk1)
   |  NONE => err_expect (opErrorMsg ops) (tkString tk)
;
fun parseEqOp fstr tk = parseOp fstr tk eqOps;
fun parseRelOp fstr tk = parseOp fstr tk relOps;
fun parseAddOp fstr tk = parseOp fstr tk addOps;
fun parseMultOp fstr tk = parseOp fstr tk multOps;
fun parseUnaryOp fstr tk = parseOp fstr tk unaryOps;
fun parseAndOp fstr tk = parseOp fstr tk andOps;
fun parseOrOp fstr tk = parseOp fstr tk orOps;
fun parseCommaOp fstr tk = parseOp fstr tk commaOps;

fun parseBinaryExpLeft fstr tk parse_opnd is_opr parse_opr =
   let
      fun parseBinaryExpLeftH tk lft =
         if is_opr tk
         then
            let
               val (opr, tk1) = parse_opr fstr tk;
               val (rht, tk2) = parse_opnd fstr tk1;
            in
               parseBinaryExpLeftH tk2 (EXP_BINARY {opr=opr, lft=lft, rht=rht})
            end
         else (lft, tk)
      ;
      val (lft, tk1) = parse_opnd fstr tk;
   in
      parseBinaryExpLeftH tk1 lft
   end
;


fun parseList inStream token parse_expr list_type is_check =
  let
    val error_msg_func = (list_err_msg list_type)
    val can_be_empty = (list_empty_permission list_type)
  in
    if can_be_empty = true then
      (parseListEmptyHelper inStream token parse_expr is_check error_msg_func [])
    else
      (parseListHelper inStream token parse_expr is_check error_msg_func [])
  end

and

parseVarDeclar inStream (token as TK_ID id) =
  case token of
    (TK_ID _) => (
      let
        val (bind, tk1) = parsePrimaryExpression inStream token
        val (expreval_opt, tk2) = 
          (
             if tk1 = TK_ASSIGN
             then (
                let val (expreval, tk2) =
                   parseAssignmentExpression inStream (nextToken inStream)
                in (SOME expreval, tk2)
                end
             )
             else (NONE, tk1)
          )
      in
        (EXP_VAR {bind = bind, expreval = expreval_opt}, tk2)
      end 
    )|
  _ => (err_expect "identifier" (tkString token)) 

and 

parseVarDeclarList inStream token =
   let val (var, tk1) = parseList inStream token parseVarDeclar "vars" isIdentifier
   in
      case var of
         [] => err_expect "identifier" (tkString token)| 
         _ => (var, tk1)
   end

and 


parseParameterList inStream token =
   parseList inStream token parsePrimaryExpression "params" isIdentifier
and 

parseParameters inStream token =
  case token of
    TK_LPAREN => (
      let 
        val (param, tk1) = parseParameterList inStream (nextToken inStream)
      in 
        (param, (match_tk inStream tk1 TK_RPAREN))
      end
    ) |
    _ => (err_expect (tkString TK_LPAREN) (tkString token))

and 

parseArgsList inStream token =
   parseList inStream token parseAssignmentExpression "args" isExpression

and 

parseArgs inStream token =
  case token of
    TK_LPAREN => (
      let 
        val (arg, tk1) = parseArgsList inStream (nextToken inStream)
      in 
        (arg, (match_tk inStream tk1 TK_RPAREN))
      end
    ) |
    _ => (err_expect (tkString TK_LPAREN) (tkString token))

and 

parseExpression fstr tk =
   parseBinaryExpLeft fstr tk parseAssignmentExpression isCommaOp parseCommaOp

and 

parseAssignmentExpression fstr tk =
   (case parseConditionalExpression fstr tk of
      (lhs, TK_ASSIGN) =>
         if isValidLHS lhs
         then
            let
               val tk1 = nextToken fstr
               val (rhs, tk2) = parseAssignmentExpression fstr tk1;
            in
               (EXP_ASSIGN {lhs=lhs, rhs=rhs}, tk2)
            end
         else error ("unexpected token '='\n")
   |  ret => ret
   )
and parseConditionalExpression fstr tk =
   (case parseLogicalOrExpression fstr tk of
      (guard, TK_QUESTION) =>
         let
            val tk1 = nextToken fstr
            val (thenExp, tk2) = parseAssignmentExpression fstr tk1;
            val tk3 = match_tk fstr tk2 TK_COLON;
            val (elseExp, tk4) = parseAssignmentExpression fstr tk3;
         in
            (EXP_COND {guard=guard, thenExp=thenExp, elseExp=elseExp}, tk4)
         end
   |  ret => ret
   )
and parseLogicalOrExpression fstr tk =
   parseBinaryExpLeft fstr tk parseLogicalAndExpression isOrOp parseOrOp
and parseLogicalAndExpression fstr tk =
   parseBinaryExpLeft fstr tk parseEqualityExpression isAndOp parseAndOp
and parseEqualityExpression fstr tk =
   parseBinaryExpLeft fstr tk parseRelationalExpression isEqOp parseEqOp
and parseRelationalExpression fstr tk =
   parseBinaryExpLeft fstr tk parseAdditiveExpression isRelOp parseRelOp
and parseAdditiveExpression fstr tk =
   parseBinaryExpLeft fstr tk parseMultiplicativeExpression isAddOp parseAddOp
and parseMultiplicativeExpression fstr tk =
   parseBinaryExpLeft fstr tk parseUnaryExpression isMultOp parseMultOp
and parseUnaryExpression fstr tk =
   if isUnaryOp tk
   then
      let
         val (opr, tk1) = parseUnaryOp fstr tk;
         val (opnd, tk2) = parseLeftHandSideExpression fstr tk1;
      in
         (EXP_UNARY {opr=opr, opnd=opnd}, tk2)
      end
   else parseLeftHandSideExpression fstr tk
and 
parseLeftHandSideExpression fstr tk =
   parseCallExpression fstr tk

and

parseCallExprHelper inStream token member = 
  if token = TK_LPAREN then
      let 
        val (args, tk1) = parseArgs inStream token
      in 
        (parseCallExprHelper inStream tk1 (EXP_CALL {member=member, args=args}))
      end
  else 
    (member, token)

and 

parseCallExpression inStream token =
   let
      val (member, tk1) = parseMemberExpression inStream token
   in
      if tk1 = TK_LPAREN then 
        (parseCallExprHelper inStream tk1 member)
      else 
        (member, tk1)
   end

and 

parseMemberExpression fstr tk =
   parsePrimaryExpression fstr tk

and 

parsePrimaryExpression fstr (tk as TK_LPAREN) =
   let
      val tk1 = match_tk fstr tk TK_LPAREN;
      val (exp, tk2) = parseExpression fstr tk1;
      val tk3 = match_tk fstr tk2 TK_RPAREN;
   in
      (exp, tk3)
   end
  | parsePrimaryExpression fstr (tk as TK_ID id) =
   (EXP_ID id, #2 (match_id fstr tk))
  | parsePrimaryExpression fstr (tk as TK_NUM n) =
   (EXP_NUM n, #2 (match_num fstr tk))
  | parsePrimaryExpression fstr (tk as TK_TRUE) =
   (EXP_TRUE, match_tk fstr tk TK_TRUE)
  | parsePrimaryExpression fstr (tk as TK_FALSE) =
   (EXP_FALSE, match_tk fstr tk TK_FALSE)
  | parsePrimaryExpression fstr (tk as TK_UNDEFINED) =
   (EXP_UNDEFINED, match_tk fstr tk TK_UNDEFINED)
  | parsePrimaryExpression fstr (tk as TK_STRING s) =
   (EXP_STRING s, #2 (match_string fstr tk))
  | parsePrimaryExpression fstr (tk as TK_FUNCTION) =
   parseFunctionExpression fstr tk
  | parsePrimaryExpression fstr tk =
   err_expect "value" (tkString tk)

and 

parseFunctExprBind inStream token = 
  if (isIdentifier token) then
    let 
      val (bind2, tk1) = parsePrimaryExpression inStream token
    in 
      (SOME bind2, tk1)
    end
  else 
    (NONE, token)

and

parseFunctionExpression inStream token =
   let
      val tk1 = match_tk inStream token TK_FUNCTION
      val (bind1, tk2) = parseFunctExprBind inStream tk1
      val (params, tk3) = parseArgs inStream tk2
    in
      let
        val tk4 = match_tk inStream tk3 TK_LBRACE
        val (body, tk5) = parseSourceElements inStream tk4
        val tk6 = match_tk inStream tk5 TK_RBRACE
      in
        (EXP_FUNCT {bind = bind1, params = params, body = body}, tk6)
      end
   end

and

(* statement parsing functions *)

parseExpressionStatement inStream token = 
  case token of
  TK_FUNCTION => (error "cannot begin with function") |
  _ => (
      let
        val (expression, tk1) = parseExpression inStream token;
        val tk2 = match_tk inStream tk1 TK_SEMI;
      in
        (ST_EXP {exp=expression}, tk2)
      end
  )

and

parseStatement fstr (tk as TK_LBRACE) = parseBlockStatement fstr tk
  | parseStatement fstr (tk as TK_IF) = parseIfStatement fstr tk
  | parseStatement fstr (tk as TK_PRINT) = parsePrintStatement fstr tk
  | parseStatement fstr (tk as TK_WHILE) = parseWhileStatement fstr tk
  | parseStatement fstr (tk as TK_VAR) = parseVariableStatement fstr tk
  | parseStatement fstr (tk as TK_RETURN) = parseReturnStatement fstr tk
  | parseStatement fstr tk =
   if isExpression tk
   then parseExpressionStatement fstr tk
   else err_expect "statement" (tkString tk)
and parseBlockStatement fstr tk =
   let
      val tk1 = match_tk fstr tk TK_LBRACE;
      val (lst, tk2) = parseRepetition fstr tk1 isStatement parseStatement;
      val tk3 = match_tk fstr tk2 TK_RBRACE;
   in
      (ST_BLOCK {stmts=lst}, tk3)
   end
and parseIfStatement fstr tk =
   let
      val tk1 = match_tk fstr tk TK_IF;
      val tk2 = match_tk fstr tk1 TK_LPAREN;
      val (guard, tk3) = parseExpression fstr tk2;
      val tk4 = match_tk fstr tk3 TK_RPAREN;
      val (th, tk5) = parseBlockStatement fstr tk4;
      val (el, tk6) = parseElse fstr tk5;
   in
      (ST_IF {guard=guard, th=th, el=el}, tk6)
   end
and parseElse fstr (tk as TK_ELSE) =
   let
      val tk1 = match_tk fstr tk TK_ELSE;
      val (el, tk2) = parseBlockStatement fstr tk1;
   in
      (el, tk2)
   end
  | parseElse fstr tk =
      (ST_BLOCK {stmts=[]}, tk)
and parsePrintStatement fstr tk =
   let
      val tk1 = match_tk fstr tk TK_PRINT;
      val (exp, tk2) = parseExpression fstr tk1;
      val tk3 = match_tk fstr tk2 TK_SEMI;
   in
      (ST_PRINT {exp=exp}, tk3)
   end
and parseWhileStatement fstr tk =
   let
      val tk1 = match_tk fstr tk TK_WHILE;
      val tk2 = match_tk fstr tk1 TK_LPAREN;
      val (guard, tk3) = parseExpression fstr tk2;
      val tk4 = match_tk fstr tk3 TK_RPAREN;
      val (body, tk5) = parseBlockStatement fstr tk4;
   in
      (ST_WHILE {guard=guard, body=body}, tk5)
   end

and 

parseVariableStatement inStream token =
  let
    val tk1 = match_tk inStream token TK_VAR
  in
    let
      val (variables, tk2) = (parseVarDeclarList inStream tk1)
      val tk3 = (match_tk inStream tk2 TK_SEMI)
    in
      ((ST_VAR {var = variables}), tk3)
    end
  end

and 

parseFuncDeclar inStream token =
  let 
    val tk1 = match_tk inStream token TK_FUNCTION
  in
    case tk1 of
    (TK_ID _) =>
      (
          let
              val (bind, tk2) = parsePrimaryExpression inStream tk1
              val (params, tk3) = parseParameters inStream tk2
          in
            let
              val tk4 = match_tk inStream tk3 TK_LBRACE
              val (body, tk5) = parseSourceElements inStream tk4
            in
              let
                val tk6 = match_tk inStream tk5 TK_RBRACE
              in
                (
                  FUNCT_DECLAR {
                    funct = (
                      EXP_FUNCT {
                        bind = SOME bind,
                        params = params,
                        body = body
                      }
                    )
                  }, tk6)
              end
            end
          end
        ) |
      _ => (err_expect "identifier" (tkString tk1))
   end

and 

parseReturnStatement inStream token =
  let 
    val tk1 = match_tk inStream token TK_RETURN
  in
    case tk1 of
    TK_SEMI => ((ST_RET {exp = NONE}), (nextToken inStream)) |
    _ => (
        if (isExpression tk1) then 
          (
            let 
              val (express, tk2) = parseExpression inStream tk1
            in 
              ((ST_RET {exp = SOME express}), (match_tk inStream tk2 TK_SEMI))
            end
          )
        else 
          (err_expect ";" (tkString tk1))
    )
end

and

parseSourceElement fstr tk =
  if tk = TK_FUNCTION then 
    (parseFuncDeclar fstr tk)
  else if (isStatement tk) then 
    (
      let 
        val (stmt, tk1) = parseStatement fstr tk;
      in 
        (STMT {stmt=stmt}, tk1)
      end
    )
  else 
    (err_expect "statement or function declaration" (tkString tk))

and 

parseSourceElements fstr tk =
   parseRepetition fstr tk isSourceElement parseSourceElement
;

fun parseProgram fstr tk =
   let
      val (elems, tk1) = parseSourceElements fstr tk;
      val _ = match_eof fstr tk1;
   in
      PROGRAM {elems=elems}
   end

fun parseStream fstr =
      parseProgram fstr (nextToken fstr)

fun parse file =
   let
      val fstr = openIn(file)
         handle oops =>
            (output (stdErr, "cannot open file: " ^ file ^ "\n");
            OS.Process.exit OS.Process.failure)
   in
      parseStream fstr
   end
;
