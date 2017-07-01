val key_chars = ["else", "false", "function", "if", "new", "print" , "return" , "this" , "true", "typeof" , "undefined" , "var", "while"]; 
val symbol_chars = ["{", "}", "(", ")", "[", "]", ".", ",", ":", ";", "?", "+", "-", "*", "/", "%", "&"];
val esc_chars = [#"\"", #"\\", #"b", #"f", #"n", #"r", #"t", #"v"]


fun advSeq inStream = 
   TextIO.input1(inStream)
;

fun printNumSeq inStream charStr cat = (
   advSeq(inStream);
   case TextIO.lookahead(inStream) of
 SOME (c) => 
   if (Char.isDigit(c)) then 
      (printNumSeq inStream (charStr ^ Char.toString(c)) cat)
   else
      ((TextIO.print cat); (print charStr); (print "\n"))
)

fun checkKeyword charStr (x::xs) =
   if null xs then 
      if x = charStr then 
      (
         TextIO.print("keyword: "); 
         TextIO.print charStr; 
         TextIO.print("\n")
      ) 
      else 
      (
         TextIO.print("identifier: "); 
         TextIO.print charStr; 
         TextIO.print("\n")
      )
   else if x = charStr then 
      (
         TextIO.print("keyword: "); 
         TextIO.print charStr;
         TextIO.print("\n")) 
   else 
      (checkKeyword charStr xs)

;

fun printCharSeq inStream s = (
   advSeq(inStream);
   case TextIO.lookahead(inStream) of
      SOME c => 
         if (Char.isAlpha(c)) then 
            (printCharSeq inStream (s ^ Char.toString(c)))
         else if (Char.isDigit(c)) then 
            (printNumSeq inStream (s ^ Char.toString(c)) ("identifier: "))
         else 
            (checkKeyword s key_chars)
)  

fun removeSpc inStream = (
   advSeq(inStream);
   case TextIO.lookahead(inStream) of
      SOME c =>
         if (Char.isSpace(c)) then 
            (removeSpc inStream) 
         else 
            () 
      | 
      NONE =>
   TextIO.print("end-of-file\n")
)


fun printSymSeq inStream charStr = (
   advSeq(inStream);
   if charStr = "&" then
      if TextIO.lookahead(inStream) = (SOME #"&") then 
      (
         advSeq(inStream);
         TextIO.print("symbol: &&\n")) 
      else 
         (print("invalid symbol: '" ^ (charStr) ^ "'\n"); OS.Process.exit(OS.Process.failure))
   else if charStr = "|" then
      if TextIO.lookahead(inStream) = (SOME #"|") then 
         (advSeq(inStream);TextIO.print("symbol: ||\n")) 
      else 
         (print("invalid symbol: '" ^ (charStr) ^ "'\n");OS.Process.exit(OS.Process.failure))
      else if charStr = ">" then
         if TextIO.lookahead(inStream) = (SOME #"=") then 
            (advSeq(inStream); TextIO.print("symbol: >=\n")) 
         else print("symbol: >\n")
      else if charStr = "<" then
         if TextIO.lookahead(inStream) = (SOME #"=") then 
            (advSeq(inStream); TextIO.print("symbol: <=\n")) 
         else print("symbol: <\n")
      else if charStr = "=" then
         if TextIO.lookahead(inStream) = (SOME #"=") then 
            (advSeq(inStream); TextIO.print("symbol: ==\n")) 
         else print("symbol: =\n")
      else if charStr = "!" then
         if TextIO.lookahead(inStream) = (SOME #"=") then 
            (advSeq(inStream); TextIO.print("symbol: !=\n")) 
         else print("symbol: !\n")
      else
      (
         if (List.exists (fn x => x = charStr) symbol_chars) then
            (TextIO.print("symbol: "); (print (charStr)); print("\n"))
         else
           (TextIO.print("invalid symbol: '" ^ charStr ^ "'\n"); OS.Process.exit(OS.Process.failure)) 
      )
   
)  

fun printStrSeq inStream s = (
   advSeq(inStream);
   case TextIO.lookahead(inStream) of
      SOME c => 
         if (c = #"\"") then 
            (
               advSeq(inStream); 
               TextIO.print("string: "); 
               TextIO.print(s ^ "\""); 
               TextIO.print("\n"))
         else if (c = #"\\") then 
            (
               advSeq(inStream);
               if (List.exists (fn x => x = (valOf(TextIO.lookahead(inStream)))) esc_chars) then
                  (printStrSeq inStream (s ^ "\\" ^ Char.toString(valOf(TextIO.lookahead(inStream))))) 
               else 
                  (
                     TextIO.print("invalid escape sequence: '" ^ "\\" ^ Char.toString(valOf(TextIO.lookahead(inStream))) ^ "'\n");
                     OS.Process.exit(OS.Process.failure)
                  )
            )
      else (printStrSeq inStream (s ^ (Char.toString(valOf(TextIO.lookahead(inStream))))))  
      | NONE =>
         (
            TextIO.print("string not terminated\n");
            OS.Process.exit(OS.Process.failure))
)
;

fun recognizeToken (inStream: TextIO.instream) =
   case TextIO.lookahead(inStream) of
   SOME c =>
      if (Char.isDigit(c)) then 
         (printNumSeq inStream (Char.toString(c)) ("number: "))
      else if (Char.isAlpha(c)) then 
         (printCharSeq inStream (Char.toString(c)))
      else if (Char.isSpace(c)) then 
         (removeSpc inStream)
      else if (c = #"\"") then 
         (printStrSeq inStream "\"")
      else 
         (printSymSeq inStream (Char.toString(c)))
   |
   NONE =>
      TextIO.print("end-of-file\n")
;  
