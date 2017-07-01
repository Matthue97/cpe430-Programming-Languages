(* CPE430 Assignment 4 Kevin Yang*)

use "ast.sml";
use "recognizer.sml"; 
use "tokenizer.sml"; 

fun tokStr token = 
  case token of
    TK_LBRACE => " {" |
    TK_RBRACE => " }" |
    TK_LPAREN => "(" |
    TK_RPAREN => ")" |
    TK_LBRACKET => "[" |
    TK_RBRACKET => "]" |
    TK_COMMA => "," |
    TK_SEMI => ";" |
    TK_QUESTION => "?" | 
    TK_COLON => ":" |
    TK_DOT => "." |
    TK_PLUS => "+" |
    TK_MINUS => "-" |
    TK_TIMES => "*" |
    TK_DIVIDE => "/" |
    TK_MOD => "%" |
    TK_AND => "&&" |
    TK_OR => "||" |
    TK_ASSIGN => "=" |
    TK_EQ => "==" |
    TK_LT => "<" |
    TK_LE => "<=" |
    TK_GT => ">" |
    TK_GE => ">=" |
    TK_NOT => "!" |
    TK_NE => "!=" |
    TK_ELSE => "else" |
    TK_FALSE => "false" |
    TK_FUNCTION => "function" |
    TK_IF => "if" |
    TK_NEW => "new" |
    TK_PRINT => "print" |
    TK_RETURN => "return" |
    TK_THIS => "this" |
    TK_TRUE => "true" |
    TK_TYPEOF => "typeof" |
    TK_UNDEFINED => "undefined" |
    TK_VAR => "var" |
    TK_WHILE => "while" |
    (TK_NUM(x)) => Int.toString(x) |
    (TK_ID(x)) => x |
    (TK_STRING(x)) => x |
    TK_EOF => "eof"
;

fun parseTok inStream token tokType = 
  case tokType of
  "OR" => ((nextToken inStream), BOP_OR) |
  "AND" => ((nextToken inStream), BOP_AND) |
  "COMMA" => ((nextToken inStream),BOP_COMMA) |
  " " => ((nextToken inStream), BOP_NONE) |
  tokType => ((nextToken inStream), BOP_NONE)
;

fun isOperator token opType = 
  case opType of
  "Unary" => (
    if ((token = TK_MINUS) orelse (token = TK_NOT) orelse (token = TK_TYPEOF)) then
      true
    else
      false
  )|
  "Add" => (
    if ((token = TK_PLUS) orelse (token = TK_MINUS)) then
      true
    else
      false
  )|
  "Mult" => (
    if ((token = TK_TIMES) orelse (token = TK_DIVIDE) orelse (token = TK_MOD)) then
      true
    else
      false 
  )|
  "Eq" => (
    if ((token = TK_EQ) orelse (token = TK_NE)) then
      true
    else
      false
  )|
  "Rel" => (
    if ((token = TK_GE) orelse (token = TK_GT) orelse (token = TK_LE) orelse (token = TK_LT)) then
      true
    else
      false 
  )
;

fun primeExpr inStream token = 
  case token of 
  TK_TRUE => ((nextToken inStream), EXP_TRUE) |
  TK_FALSE =>((nextToken inStream), EXP_FALSE) |
  TK_LPAREN => (
    let
      val (nTok, lft) = (condExpr inStream (nextToken inStream));
      val (nTok, expr) = (recurseExpr inStream nTok lft)
    in 
      if(nTok = TK_RPAREN) then 
        ((nextToken inStream), expr) 
      else 
        (output (TextIO.stdErr, "expected ')', found '" ^ (tokStr token) ^ "'\n");OS.Process.exit(OS.Process.failure)) 
    end 
    ) |
  TK_UNDEFINED => ((nextToken inStream),EXP_UNDEFINED) |
  TK_NUM(x) => ((nextToken inStream), (EXP_NUM x)) |
  TK_STRING(x) => ((nextToken inStream), (EXP_STRING x)) |
  TK_ID(id) => ((nextToken inStream), (EXP_ID id)) |
  token => (output (TextIO.stdErr, "expected 'value', found '" ^  (tokStr token) ^ "'\n");OS.Process.exit(OS.Process.failure))   

and 

recurseExpr inStream token lft =
  if(token = TK_COMMA) then
    let
      val (nTok, opr) = (parseTok inStream token "COMMA")
      val (n2Tok,rht) = (condExpr inStream nTok)
    in
      (recurseExpr inStream n2Tok (EXP_BINARY {opr = opr, lft = lft, rht = rht}))
    end
  else
    (token ,lft)

and

lorExpr inStream token =
  let
    val (nTok, opnd) = (landExpr inStream token)
  in
    (recurseAndOr inStream nTok opnd)
  end

and

landExpr inStream token =
  let
    val (nTok, opnd) = (eqExpr inStream token)
  in
    (recurseAndOr inStream nTok opnd)
  end

and

recurseAndOr inStream token leftExpr = 
  case token of
  TK_AND => (
    let 
      val (nTok, operator) = (parseTok inStream token "AND")
      val (n2Tok, rightExpr) = (eqExpr inStream nTok)
    in
      (recurseAndOr inStream n2Tok (EXP_BINARY {opr = operator, lft = leftExpr, rht = rightExpr}))
    end
  ) |
  TK_OR => (
    let 
      val (nTok, operator) = (parseTok inStream token "OR")
      val (n2Tok, rightExpr) = (landExpr inStream nTok)
    in
      (recurseAndOr inStream n2Tok (EXP_BINARY {opr = operator, lft = leftExpr, rht = rightExpr}))
    end
  ) |
  token => (token, leftExpr)

and

idMatches (EXP_ID(x)) = true | idMatches token = false

and

tokenValid tok =  
  case tok of
   TK_TRUE => true | 
   TK_FALSE => true |
   TK_MINUS => true |
   TK_NOT => true |
   TK_NUM(tok) => true |
   TK_LPAREN => true |
   TK_STRING(tok) => true |
   TK_TYPEOF => true | 
   TK_UNDEFINED => true |
   tok => false

and

assignExpr inStream token = 
  let
    val (tok0, idCond) = (condExpr inStream token)
  in
    if (tok0 = TK_ASSIGN) then
        let
          val (tok1, throwaway) = (parseTok inStream tok0  " ")
          val (tok2, assignKeyword) = (assignExpr inStream tok1)
        in
          if (idMatches idCond) then
            (tok2, (EXP_ASSIGN {cond = idCond, assign = assignKeyword}))
          else
            (output(TextIO.stdErr, "unexpected token '='\n") ; OS.Process.exit(OS.Process.failure))
        end
    else
      (tok0, idCond)
  end

and

genStmt inStream token = 
  case token of
    TK_IF => (
      let
        val (ifTok, ifBlock) = (ifStmt inStream token)
      in
        (ifTok, ifBlock)
      end
    ) |
    TK_WHILE => (
      let
        val (whileTok, whileBlock) = (whileStmt inStream token)
      in
        (whileTok, whileBlock)
      end
    ) |
    TK_LBRACE => (
      let
        val (braceTok, braceBlock) = (blockStmt inStream token)
      in
        (braceTok, (ST_BLOCK {block = braceBlock}))
      end
    ) |
    TK_PRINT => (
      let
          val (printTok, printBlock) = (printStmt inStream token)
      in
          (printTok, printBlock)
      end
    ) |
    token => (
      let
          val (expTok, expBlock) = (exprStmt inStream token)
      in
          (expTok, (ST_EXP {exp = expBlock}))
      end
    )

and

genExpression inStream token =
  let
    val (expTok, left) = (assignExpr inStream token)
  in
    (recurseExpr inStream expTok left)
  end 

and


recurseBlock inStream token blkList = 
  if (tokenValid token) then
    let
      val (tok0, statem) = (genStmt inStream token)
    in
      (recurseBlock inStream tok0 (blkList @ [statem]))
    end
  else if token = TK_RBRACE then
    (
      let
        val (tok, throwaway) = (parseTok inStream token " ")
      in
        (tok, blkList)
      end
    )
  else
    (output (TextIO.stdErr,"expected '}', found '" ^ (tokStr token) ^ "'\n");OS.Process.exit(OS.Process.failure))

and

blockStmt inStream token = 
  if token = TK_LBRACE then
    let
      val (tok0, throwaway) = (parseTok inStream token  " ")
      val (tok1, blk) = (recurseBlock inStream tok0 [])
    in
      (tok1, blk)
    end
  else
    (output (TextIO.stdErr,"expected '{', found '" ^ (tokStr token) ^ "'\n");OS.Process.exit(OS.Process.failure))

and

ifStmt inStream token = 
  let
    val (tok0, throwaway) = (parseTok inStream token  " ")
  in
    if tok0 = TK_LPAREN then
      let
        val (exprTok, expr) = (genExpression inStream token)
        val (thenTok, tBlock) = (blockStmt inStream exprTok)
      in
        if thenTok = TK_ELSE then
          let
            val (tok1, throwaway) = (parseTok inStream thenTok  " ")
            val (elseTok, eBlock) = (blockStmt inStream tok1)
            val stThen = (ST_BLOCK {block = tBlock})
            val stElse = (ST_BLOCK {block = eBlock})
            val ifSt = (ST_IF {guard = expr, thenStatem = stThen, elseStatem = stElse})
          in
            (elseTok, ifSt)
          end
        else
          let
            val stThen = (ST_BLOCK {block = tBlock})
            val stElse = (ST_BLOCK {block = []})
            val ifSt = (ST_IF {guard = expr, thenStatem = stThen, elseStatem = stElse})
          in
            (thenTok, ifSt)
          end
      end
    else
      (output (TextIO.stdErr,"expected '(', found '" ^ (tokStr tok0) ^ "'\n");OS.Process.exit(OS.Process.failure))
    end

and

whileStmt inStream token = 
  let
    val (tok0, throwaway) = (parseTok inStream token  " ")
  in
    if tok0 = TK_LPAREN then
      let
        val (exprTok, expr) = (genExpression inStream tok0)
        val (thenTok, thenBlock) = (blockStmt inStream exprTok)
        val whileSt = (ST_WHILE {guard = expr, thenWhile = (ST_BLOCK {block = thenBlock})})
      in
        (thenTok, whileSt)
      end
    else
      (output (TextIO.stdErr,"expected '(', found '" ^ (tokStr tok0) ^ "'\n");OS.Process.exit(OS.Process.failure))
  end

and

printStmt inStream token = 
  let
    val (tok0, throwaway) = (parseTok inStream token " ")
    val (exprTok, expr) = (genExpression inStream tok0)
  in
    if exprTok = TK_SEMI then
      let
        val (tok1, throwaway) = (parseTok inStream exprTok " ")
        val printSt = (ST_PRINT {expr = expr})
      in
        (tok1, printSt)
      end
    else
      (output (TextIO.stdErr,"expected ';', found '" ^ (tokStr exprTok) ^ "'\n") ;OS.Process.exit(OS.Process.failure))
  end

and

condExpr inStream token = 
  let
    val (nTok, gExpr) = (lorExpr inStream token)
  in
    if(nTok = TK_QUESTION) then
      let
        val (n2Tok, throwAway) = (parseTok inStream nTok " ")
        val (n3Tok, thenExpr) = (assignExpr inStream n2Tok)
      in
        if(n3Tok = TK_COLON) then
          let
            val (n4Tok, throwAway) = (parseTok inStream n3Tok " ")
            val (n5Tok, elseExpr) = (assignExpr inStream n4Tok)
          in
            (n5Tok,(EXP_COND {guard = gExpr, thenExp = thenExpr, elseExp = elseExpr}))
          end
        else (output(TextIO.stdErr,"expected ':', found '" ^ (tokStr n3Tok) ^ "'\n") ; OS.Process.exit(OS.Process.failure)) 
      end
    else
      (nTok, gExpr)
  end   

and

eqExpr inStream token =
  let
    val (nTok, operand) = (relExpr inStream token)
  in
    (recurseEq inStream nTok operand)
  end

and

recurseEq inStream token leftExpr = 
if (isOperator token "Eq") then
    let
      val (nTok, operator) = (
        case token of
        TK_EQ => ((nextToken inStream),BOP_EQ) |
        TK_NE => ((nextToken inStream),BOP_NE) |
        token => OS.Process.exit(OS.Process.failure)
      ); 
      val (n2Tok, rightExpr) = (relExpr inStream nTok)
    in
      (recurseEq inStream n2Tok (EXP_BINARY {opr = operator, lft = leftExpr, rht = rightExpr}))
    end
  else
    (token, leftExpr)

and

memberExpr inStream token = 
  (primeExpr inStream token) 

and  

callExpr inStream token = 
  (memberExpr inStream token) 

and 
 
leftExprFunc inStream token = 
  (callExpr inStream token) 

and 
 
unaryExpr inStream token =  
  if (isOperator token "Unary") then
    let  
      val (nTok, operator) = (
        case token of
        TK_MINUS => ((nextToken inStream), UOP_MINUS) |
        TK_NOT => ((nextToken inStream), UOP_NOT) |
        TK_TYPEOF => ((nextToken inStream), UOP_TYPEOF) |
        token => OS.Process.exit(OS.Process.failure)
      ); 
      val (n2Tok, operand) = (leftExprFunc inStream nTok) 
    in 
      (n2Tok, (EXP_UNARY {opr = operator, opnd = operand }))     
    end 
  else   
    (leftExprFunc inStream token)  

and

multExpr inStream token =
  let
    val (nTok, operand) = (unaryExpr inStream token)
  in
    (recurseMult inStream nTok operand)
  end   
and

recurseMult inStream token leftExpr = 
  if (isOperator token "Mult") then
    let
      val (nTok, operator) = (
        case token of
        TK_MOD => ((nextToken inStream), BOP_MOD) |
        TK_TIMES => ((nextToken inStream), BOP_TIMES) |
        TK_DIVIDE => ((nextToken inStream), BOP_DIVIDE) |
        token => OS.Process.exit(OS.Process.failure)
      ); 
      val (n2Tok, rightExpr) = (unaryExpr inStream nTok)
    in
      (recurseMult inStream n2Tok (EXP_BINARY {opr = operator, lft = leftExpr, rht = rightExpr}))
    end
  else
    (token, leftExpr)

and

addExpr inStream token =
  let
    val (nTok, operand) = (multExpr inStream token)
  in
    (recurseAdd inStream nTok operand)
  end   

and

recurseAdd inStream token leftExpr = 
  if (isOperator token "Add") then
    let
      val (nTok, operand) = (
        case token of
        TK_PLUS => ((nextToken inStream),BOP_PLUS) |
        TK_MINUS => ((nextToken inStream), BOP_MINUS) |
        token => OS.Process.exit(OS.Process.failure)
      ); 
      val (n2Tok, rightExpr) = (multExpr inStream nTok)
    in
      (recurseAdd inStream n2Tok (EXP_BINARY {opr = operand, lft = leftExpr, rht = rightExpr}))
    end
  else
    (token, leftExpr)

and

relExpr inStream token =
  let
    val (nTok, operand) = (addExpr inStream token)
  in
    (recurseRel inStream nTok operand)
  end

and

recurseRel inStream token leftExpr = 
  if (isOperator token "Rel") then
    let
      val (nTok, operator) = (
        case token of
        TK_GE => ((nextToken inStream), BOP_GE) |
        TK_GT => ((nextToken inStream), BOP_GT) |
        TK_LE => ((nextToken inStream), BOP_LE) |
        TK_LT => ((nextToken inStream), BOP_LT) |
        token => OS.Process.exit(OS.Process.failure)
      );
      val (n2Tok, rightExpr) = (addExpr inStream nTok)
    in
      (recurseRel inStream n2Tok (EXP_BINARY {opr = operator, lft = leftExpr, rht = rightExpr}))
    end
  else
    (token, leftExpr)

and

exprStmt inStream token = 
  let
    val (nTok, leftExpr) = (condExpr inStream token);
    val (nTok, expr) = (recurseExpr inStream nTok leftExpr)
  in
    case nTok of
      TK_SEMI => (
        let
          val (pTok, throwaway) = (parseTok inStream nTok " ")
        in
          (pTok, expr)
        end
      ) |
      nTok => ((TextIO.output (TextIO.stdErr,"expected ';', found '" ^ (tokStr nTok) ^ "'\n");
        OS.Process.exit(OS.Process.failure)))
  end 
;

fun validTokProcess inStream token = 
   let
      val (nTok, express) = (exprStmt inStream token);
      val statem = (STMT { stmt = (ST_EXP {exp = express})})
   in
      (nTok, statem)
   end
;

fun checkEOF inStream token = 
   case token of 
      TK_EOF => [] |
      token => (TextIO.output(TextIO.stdErr,"expected 'eof', found '" ^ (tokStr token) ^ "'\n"); 
        OS.Process.exit(OS.Process.failure))
;

fun recurseProg inStream token =
   let
      val tokMatch = fn tok => case tok of
         TK_TRUE => true | 
         TK_FALSE => true |
         TK_MINUS => true |
         TK_NOT => true |
         TK_NUM(tok) => true |
         TK_LPAREN => true |
         TK_STRING(tok) => true |
         TK_TYPEOF => true | 
         TK_UNDEFINED => true |
         TK_IF => true |
         TK_WHILE => true |
         TK_LBRACE => true |
         TK_PRINT => true |
         TK_ID(x)=> true |
         tok => false
         
   in
      let
         val validTok = (tokMatch token)
      in
         case validTok of 
            true => (
            let
               val (nTok, source) = (validTokProcess inStream token)
               val result = (recurseProg inStream nTok)
            in
               source::result
            end
            ) |
            false => (checkEOF inStream token)
      end
   end
;

fun parse fileStr =
   let
      val inStream = TextIO.openIn(fileStr)
   in
      PROGRAM {elems = (recurseProg inStream (nextToken inStream))}
   end
; 