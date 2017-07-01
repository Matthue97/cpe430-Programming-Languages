open TextIO;
val OPTIONAL = true;

exception InvalidSymbol of string;
exception InvalidString;
exception UnterminatedString;
exception InvalidEscapeSequence of string;

val keywords = ["else", "false", "function", "if", "new", "print",
   "return", "this", "true", "typeof", "undefined", "var", "while"];

fun error s = (output (stdErr, s); OS.Process.exit OS.Process.failure);

fun member s xs = List.exists (fn st => st = s) xs;

fun pairLookup s xs =
   case List.find (fn (st, _) => st = s) xs of
      NONE => NONE
   |  SOME (_, v) => SOME v
;

fun streamReduce pred func base fstr =
   case lookahead fstr of
      NONE => base
   |  SOME c => if pred c
         then (input1 fstr; streamReduce pred func (func (c, base)) fstr)
         else base
;

val clearWhitespace = streamReduce Char.isSpace (fn _ => ()) ();
fun buildToken pred fstr = implode (rev (streamReduce pred (op ::) [] fstr));

fun outputCategory cat s = (output (stdOut, cat ^ s ^ "\n"); ()) ;

fun outputIdentifier id =
   if member id keywords
      then outputCategory "keyword: " id
      else outputCategory "identifier: " id
;

val outputNumber = outputCategory "number: ";
fun outputString s = outputCategory "string: " ("\"" ^ (String.toString s) ^ "\""); 
val outputSymbol = outputCategory "symbol: "; 

val recognizeIdentifier = buildToken Char.isAlphaNum;
val recognizeNumber = buildToken Char.isDigit;

val escapeSequenceList =
   [
    (#"\"", #"\""),
    (#"\\", #"\\"),
    (#"b", #"\b"),
    (#"f", #"\f"),
    (#"n", #"\n"),
    (#"r", #"\r"),
    (#"t", #"\t"),
    (#"v", #"\v")
   ]
;

fun buildEscapeCharacter fstr =
   case input1 fstr of
      SOME c =>
         (case (pairLookup c escapeSequenceList) of
             SOME es => es
           | NONE => raise InvalidEscapeSequence ("\\" ^ (str c))
         )
   |  NONE => raise UnterminatedString
;

fun buildString fstr s =
   case input1 fstr of
      SOME #"\"" => implode (rev s)
   |  SOME #"\\" => buildString fstr ((buildEscapeCharacter fstr) :: s)
   |  SOME (#"\n" | #"\r" | #"\f") => raise UnterminatedString
   |  SOME c => buildString fstr (c :: s)
   |  NONE => raise UnterminatedString
;

fun recognizeString fstr =
   case input1 fstr of
      SOME #"\"" => buildString fstr []
   |  x => raise InvalidString
;

fun buildSymbol need optional fstr s =
   let
      val input = lookahead fstr
   in
      if (isSome input) andalso (member (valOf input) need)
      then (input1 fstr; (s ^ (str (valOf input))))
      else if optional
           then s
           else raise InvalidSymbol s
   end
;

val symbolBuildList =
   let
      fun simple_symbol fstr s = s;
   in
      [
       (#"{", simple_symbol),
       (#"}", simple_symbol),
       (#"(", simple_symbol),
       (#")", simple_symbol),
       (#"[", simple_symbol),
       (#"]", simple_symbol),
       (#",", simple_symbol),
       (#";", simple_symbol),
       (#"?", simple_symbol),
       (#":", simple_symbol),
       (#".", simple_symbol),
       (#"+", simple_symbol),
       (#"-", simple_symbol),
       (#"*", simple_symbol),
       (#"/", simple_symbol),
       (#"%", simple_symbol),
       (#"&", buildSymbol [#"&"] (not OPTIONAL)),
       (#"|", buildSymbol [#"|"] (not OPTIONAL)),
       (#"=", buildSymbol [#"="] OPTIONAL),
       (#"<", buildSymbol [#"="] OPTIONAL),
       (#">", buildSymbol [#"="] OPTIONAL),
       (#"!", buildSymbol [#"="] OPTIONAL)
      ]
   end
;

fun recognizeSymbol fstr =
   case input1 fstr of
      SOME c =>
         (case (pairLookup c symbolBuildList) of
             SOME build_func => build_func fstr (str c)
           | NONE => raise InvalidSymbol (str c)
         )
   |  NONE => raise InvalidSymbol "-eof-"
;

fun recognizeFirstToken fstr =
   case lookahead fstr of
      SOME c => if Char.isAlpha c
                then outputIdentifier (recognizeIdentifier fstr)
                else if Char.isDigit c
                then outputNumber (recognizeNumber fstr)
                else if c = #"\""
                then outputString (recognizeString fstr)
                else outputSymbol (recognizeSymbol fstr)
   | NONE => (output (stdOut, "end-of-file\n"); ())
;

fun recognizeToken fstr =
   (clearWhitespace fstr; recognizeFirstToken fstr)
   handle InvalidSymbol s => error ("invalid symbol: '" ^ s ^ "'\n")
        | UnterminatedString => error ("string not terminated\n")
        | InvalidString => error ("invalid string\n")
        | InvalidEscapeSequence s =>
            error ("invalid escape sequence: '" ^ s ^ "'\n")
;

