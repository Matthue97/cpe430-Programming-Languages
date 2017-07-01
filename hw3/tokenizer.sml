datatype tokens =
TK_ELSE | TK_FALSE | TK_FUNCTION | TK_IF | TK_NEW | TK_PRINT | TK_RETURN | 
TK_THIS | TK_TRUE | TK_TYPEOF | TK_UNDEFINED | TK_VAR | TK_WHILE | TK_ASSIGN | 
TK_NOT | TK_EQ | TK_NE | TK_GT | TK_LT | TK_GE | TK_LE | TK_AND | TK_OR | TK_PLUS | 
TK_MINUS | TK_TIMES | TK_DIVIDE | TK_MOD | TK_DOT | TK_LPAREN | TK_RPAREN | TK_LBRACKET | 
TK_RBRACKET | TK_LBRACE | TK_RBRACE | TK_QUESTION | TK_COLON | TK_COMMA | TK_SEMI | TK_NUM of int | 
TK_ID of string | TK_STRING of string | TK_EOF
;


val key_chars = [
"else", "false", "function", "if", "new", "print", "return",
 "this", "true", "typeof", "undefined", "var", "while"
 ]

val dbl_op_chars = [
 #"|", #"&"
]

val cmp_op_chars = [
 #">", #"<", #"=", #"!"
]

val op_chars = [
 #"(", #")", #"[", #"]", #"{", #"}", #"?", #":", #",", #";", #"%", #".",
 #"=", #"!", #">", #"<", #"&", #"|", #"+", #"-", #"*", #"/"
]

val esc_chars = [
 #"\"", #"\\", #"b", #"f", #"n", #"r", #"t", #"v"
]

fun str_fail (c: string) =
   let
      val retEOF = TK_EOF
   in
      while (true) do (
         print ("string not terminated");
         OS.Process.exit(OS.Process.failure)
      );
      retEOF
   end
;

fun esc_fail (c: char) =
   let
      val retEOF = TK_EOF
   in
      while (true) do (
         print ("invalid escape sequence: '\\" ^ (String.str c) ^ "'\n");
         OS.Process.exit(OS.Process.failure)
      );
      retEOF
   end
;

fun sym_fail (c: char) =
   let
      val retEOF = TK_EOF
   in
      while (true) do (
         print ("invalid symbol: '" ^ (String.str c) ^ "'\n");
         OS.Process.exit(OS.Process.failure)
      );
      retEOF
   end
;
