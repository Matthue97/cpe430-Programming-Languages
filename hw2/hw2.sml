(* CP430 hw2 by Kevin Yang *)
(* kyang09  *)
(* Cal Poly SLO  *)

Control.Print.printDepth := 20;
Control.Print.printLength := 100;

(* Part 1 DataType  *)
datatype 'a List = 
   ListNode of ('a * 'a List)
   | EmptyList
;


(* Part 4 DataType  *)
datatype 'a BinTree = 
   BinTreeNode of {value: 'a, lft: 'a BinTree, rht: 'a BinTree}
   | EmptyBinTree
;


(* Part 5  DataType*)
datatype 'a ThingCollection =
   OneThing of ('a * 'a ThingCollection)
   | TwoThings of ('a * 'a * 'a ThingCollection)
   | ManyThings of ('a list * 'a ThingCollection)
   | Nothing
;

(* Part 10 DataType  *)
datatype NTree = 
   NTreeNode of int * NTree list
   | EmptyNTree
;

(* Part 1  *)

fun consList (head, tail) = 
   ListNode(head, tail)
;

fun headList (ListNode(head, tail)) = 
   head
;

fun tailList (ListNode(head, tail)) = 
   tail
;


(* Part 2  *)

fun lengthList EmptyList = 
   0 |
   lengthList (ListNode(head, tail)) = 
      (lengthList tail) + 1
;

(* Part 3  *)

fun mapList func EmptyList = 
   EmptyList |
   mapList func (ListNode(head, tail)) = 
      ListNode((func head), (mapList func tail))
;

(* Part 4  *)
fun mapBinTree func EmptyBinTree = 
   EmptyBinTree |
   mapBinTree func (BinTreeNode{value = v, lft = left, rht = right}) = 
      BinTreeNode{value = func v, lft = mapBinTree func left, rht = mapBinTree func right}
;

(* Part 5  *)
fun countThingsInCollection Nothing = 
   0 |
   countThingsInCollection (OneThing(a,b)) = 
      (countThingsInCollection b) + 1 |
   countThingsInCollection (TwoThings(a,b,c)) = 
      (countThingsInCollection c) + 2 |
   countThingsInCollection (ManyThings(thingsList, z)) = 
      (countThingsInCollection z) + (length thingsList)
;

(* Part 6  *)
fun countOneThingNodes Nothing = 
   0 |
   countOneThingNodes (OneThing(a,b)) = 
      (countOneThingNodes b) + 1 |
   countOneThingNodes (TwoThings(a,b,c)) = 
      (countOneThingNodes c) + 0 |
   countOneThingNodes (ManyThings(thingsList, z)) = 
      (countOneThingNodes z) + 0
;

(* Part 7  *)
fun countNodesByPredicate func Nothing = 
   if (func Nothing)
   then 1
   else 0 |
   countNodesByPredicate func (OneThing(a,b)) = 
      if (func (OneThing(a,b)))
      then (countNodesByPredicate func b) + 1
      else (countNodesByPredicate func b) + 0 |
   countNodesByPredicate func (TwoThings(a,b,c)) = 
      if (func (TwoThings(a,b,c)))
      then (countNodesByPredicate func c) + 1
      else (countNodesByPredicate func c) + 0 |
   countNodesByPredicate func (ManyThings(thingsList, z)) = 
      if (func (ManyThings(thingsList, z)))
      then (countNodesByPredicate func z) + 1
      else (countNodesByPredicate func z) + 0
;

(* Part 8  *)
fun countTwoThingsNodes thingsCollection = 
   countNodesByPredicate (fn (TwoThings a) => true | a => false) thingsCollection
;

(* Part 9  *)
fun reduceThingCollection f baseValue Nothing = 
   baseValue |
   reduceThingCollection f baseValue (OneThing(v, c)) = 
      f (v, (reduceThingCollection f baseValue c)) |
   reduceThingCollection f baseValue (TwoThings(a,b,c)) = 
      f (a, f (b, (reduceThingCollection f baseValue c))) |
   reduceThingCollection f baseValue (ManyThings(L, c)) = 
      foldr f (reduceThingCollection f baseValue c) L
;

(* Part 10  *)

fun flattenNTree EmptyNTree = 
   [] |
   flattenNTree (NTreeNode(v, ntree)) =
      if (null ntree) = false
      then
         v::(flattenNTree (NTreeNode(v, tl(ntree))))
      else
         []
;
