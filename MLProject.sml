(* Name: John Rowan
   Class: CSC 345 T,R 11-12:15
   Description: implement functions preOrder, inOrder
                ,postOrder, and displayTree for a binary tree  *)

use "testData.sml"; (*load the testData.sml file *)
(*==========================Traversal Functions===================*)
(*  Function: preOrder
    Argument: bTree
    Returns : 'a list of bTree elements in preOrder *)
fun preOrder(empty) = nil
  | preOrder (bTree(data,left,right)) = [data] @ preOrder(left) @ preOrder(right);

(*  Function: inOrder
    Argument: bTree
    Returns : 'a list of bTree elements inOrder *)
fun inOrder(empty) = nil
  | inOrder (bTree(data,left,right)) = inOrder(left) @ [data] @ inOrder(right);

(*  Function: postOrder
    Argument: bTree
    Returns : 'a list of bTree elements in postOrder *)
fun postOrder(empty) = nil
  | postOrder (bTree(data,left,right)) = postOrder(left) @ postOrder(right) @ [data];

(*=============================Print Functions=======================*)
(*  Function: printReal,printInt,and printX
    Argument: real, int, and X respectively
    Returns : unit , prints the element
*)
fun printReal n = print(Real.toString n);

fun printInt n = print(Int.toString n);

fun printX A = print "A"
  | printX B = print "B"
  | printX C = print "C"
  | printX D = print "D"
  | printX E = print "E"
  | printX F = print "F"
  | printX G = print "G"
  | printX H = print "H";

(*==============================Display Functions=====================*)

(*  Function: tab,displaynode,dash,displaytreeindent,displayTree
    Argument: int, 'a * int * ('a -> 'b), int, bTree * int * ('a -> 'b),
              bTree * ('a -> 'b) respectively
    Returns : unit, prints the element of bTree
*)
fun tab (0) = print ""
  | tab (i) = (print "  ";tab (i-1));

fun displaynode(x,y,printfunc) = (tab y;printfunc x;print "\n");

fun dash(x) = (tab x;print "-\n");

fun displaytreeindent(empty,x,prinfun) = print ""
  | displaytreeindent (bTree(data,empty,empty),x,prinfun) = displaynode(data,x,prinfun) 
  | displaytreeindent (bTree(data,empty,right),x,prinfun) =
    (displaynode(data,x,prinfun);dash(x+1);displaytreeindent(right,x+1,prinfun))
  | displaytreeindent (bTree(data,left,empty),x,prinfun) =
    (displaynode(data,x,prinfun);displaytreeindent(left,x+1,prinfun);dash(x+1))
  | displaytreeindent(bTree(data,left,right),x,prinfun) =
    (displaynode(data,x,prinfun);displaytreeindent(left,x+1,prinfun);displaytreeindent(right,x+1,prinfun));

fun displayTree(BT,prinfun) = displaytreeindent(BT,0,prinfun);
 

