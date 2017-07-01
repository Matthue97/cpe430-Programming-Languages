(* CP430 hw1 by Kevin Yang *)
(* kyang09  *)
(* Cal Poly SLO  *)

Control.Print.printDepth := 20;
Control.Print.printLength := 100;


(* Part 1 *)
fun intToString(num:int) =
   if num < 0 then ("-" ^ (Int.toString(Int.abs num)))
   else Int.toString(Int.abs num)
;

(* Part 2 *)
fun listLength ([]) =
   0 | listLength(x::xs) = (listLength xs) + 1
;

(* Part 3  *)
fun numberOf v [] =
   0 |
   numberOf v (x::xs) = 
      if x = v
         then (numberOf v xs) + 1
      else
         numberOf v xs
;

(* Part 4  *)
fun pairSwap [] = 
   [] |
   pairSwap ((x,y)::xs) = 
      ((y,x)::pairSwap xs)
;

(* Part 5  *)
fun findPair v [] = 
   NONE | 
   findPair v ((x,y)::xs) = 
      if v = #1(x,y)
         then SOME (x,y)
      else
         findPair v xs
;

(* Part 6  *)
fun unzip [] = 
   ([], []) | 
   unzip ((x,y)::xs) = 
      let
         val (intList, charList) = unzip xs
      in
         (x::intList, y::charList)
      end
;

(* Part 7  *)

exception UnbalancedZip;

fun zip [] [] = 
   [] |
   zip (x::xs) (y::ys) = 
      if (listLength xs) <> (listLength ys)
         then raise UnbalancedZip
      else
         let
            val pairs = zip xs ys
         in
            (x,y)::pairs
         end
;

(* Part 8  *)
fun splitFilter funct [] = 
   ([], []) |
   splitFilter funct (x::xs) = 
      let
         val (evenList, oddList) = 
            splitFilter funct xs
      in
         if funct x = true
            then (x::evenList, oddList)
         else
            (evenList, x::oddList)
      end
;

(* Part 9  *)

fun subChar (pairs:(char * char) list) ([]) =
   [] | 
   subChar (pairs:(char * char) list) ((ch::character):char list) = 
      let
         val findResult = findPair ch pairs
      in
         if Option.isSome findResult
            then (#2(Option.valOf findResult))::(subChar pairs character)
         else
            ch::(subChar pairs character)
      end
;

fun sub file pairs = 
   subChar pairs (String.explode file)
;

fun printString ofile pairs = 
   let val opfile = ofile
   in
      if TextIO.endOfStream opfile
         then ()
      else
         TextIO.print (String.implode (sub (TextIO.inputAll ofile) pairs))
   end
;

fun fileSubst filename (pairs:(char * char) list) = 
   let
      val openFile = TextIO.openIn filename
   in
      printString openFile pairs
   end
;

(* Part 10  *)
fun wordsFromFile inFile = 
   let
      val ofile = TextIO.openIn inFile
   in
      ["I","didn't","have", "time."]
   end
;
