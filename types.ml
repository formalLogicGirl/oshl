(* variable or predicate symbol is a string that must begin with uppercase *)
type varOrPredSymbol = string;;

(* function symbol is a string that must begin with lowercase *)
type funSymbol = string;;

(* arity is an integer. Semantically, its the number of arguments for a function or predicate *)
type arity = int;;

type funDef = funSymbol * arity;;

type predDef = varOrPredSymbol * arity;;

(* a term is a wff of variables, constants and functions *)
type term =
  Var of varOrPredSymbol |
  Func of funSymbol * args |
  Bool of bool
 and args = term list;;			

module OrderedTerm =
  struct
    type typ = term
    let rec length = function
      | Bool b -> 1
      | Var v -> 1
      | Func(sym, args) ->
	 let rec sum acc = function
	   | [] -> acc
	   | hd :: tl -> sum (hd + acc) tl in
	 sum 1 (List.map length args)
    let rec compare t1 t2 =
      if (length t1) > (length t2) then 1 else
	if (length t1) < (length t2) then -1 else
	  let rec compareTermList l1 l2 =
	    let compFst = (compare (List.hd l1) (List.hd l2)) in
	    if compFst <> 0 then compFst else
	      compareTermList (List.tl l1) (List.tl l2) in
	  match t1 with
	  | Bool b1 -> ( match t2 with 
			 | Bool b2  -> if b1 = b2 then 0 else
					 if b1 then 1 else -1 (* 'false' < 'true' *)
			 | _ -> -1)
	  | Var v1  -> (match t2 with
			| Bool b2 -> 1
			| Var v2 -> if v1 = v2 then 0 else
				      if v1 < v2 then -1 else 1 (* lexical order of var string *)
			| Func(s, a) -> -1)
	  | Func(s1, a1) -> (match t2 with
			     | Bool b2 -> 1
			     | Var v2 -> 1
			     | Func (s2, a2) ->
				if s1 < s2 then -1 else
				  if s1 > s2 then 1 else
				    compareTermList a1 a2)
    let rec print = function
      | Bool b -> let s =
		    if b then "true" else "false" in
		  (Printf.printf "%s" s; flush stdout)
      | Var v -> (Printf.printf "%s" v; flush stdout)
      | Func(f, a) ->
	 if a = [] then
	   (Printf.printf "%s" f; flush stdout;)
	 else
	 (Printf.printf "%s(" f;
	  flush stdout;
	  (List.iter print a);
	  Printf.printf ")";
	  flush stdout;)
  end;;

let rec sum acc = function
  | [] -> acc
  | hd :: tl -> sum (hd + acc) tl;;
		      
module Function =
  struct
    (* a function definition is a function symbol and the arity of the function *)
    type t = funDef
    let symbol x = fst(x)
    let arity x = snd(x)
  end;;

module Predicate =
  struct
    (* a predicate definition is a predicate symbol and the arity of the predicate *)
    type t = predDef
    let symbol x = fst(x)
    let arity x = snd(x)
    let orderSpecified p1 p2 =
      if p1 < p2 then -1 else
	if p1 > p2 then 1 else 0
  end;;

module Func =
  struct
    type t = funDef * args
    let def x = fst(x)
    let args x = snd(x)
  end;;

module Atom =
  struct
    type typ = Predicate.t * args
    let predicate x = fst(x)
    let args x = snd(x)
    let size x = sum 1 (List.map OrderedTerm.length (args x))
    (* compare size first
       if size is same, then compare order of predicate symbol
       if predicate symbols are of the same order, then compare number of args
       if predicate symbols are of the same order and number of args are the same, then compare order of args from left to right  *)
    let compare x y =
      let l1 = size x in
      let l2 = size y in
      if l1 < l2 then -1 else
	if l1 > l2 then 1 else
	  let p1 = Predicate.symbol x in
	  let p2 = Predicate.symbol y in
	  if (Predicate.orderSpecified p1 p2) <> 0 then (Predicate.orderSpecified p1 p2) else
	    let rec compareArgsLeftToRight a1 a2 =
	      if a1 = [] then 0
	      else
		let result = (OrderedTerm.compare (List.hd a1) (List.hd a2)) in
		if result <> 0 then result else
		  (compareArgsLeftToRight (List.tl a1) (List.tl a2)) in
	    (compareArgsLeftToRight (args x) (args y))

    let print x =
      let sym = (Predicate.symbol (predicate x)) in
      (Printf.fprintf stdout " %s(" sym);
      let rec printArgs = function
	|[] -> (Printf.fprintf stdout "")
	|hd::_ as tl -> (OrderedTerm.print hd;
			  printArgs tl) in
      printArgs (args x);
      (Printf.fprintf stdout ")\n");
      flush stdout
  end;;

type sign = string;;

module RelevantAtom =
  struct
    type typ = sign * Atom.typ
    let relevance x = fst(x)
    let atom x = snd(x)
  end;;
  

(* TODO *)
(* sign is "+" or "-" *)
type literal = sign * RelevantAtom.typ;;

  (* print_func *)
  (* print_var *)
  (* union_lists *)

  (* atomBooltuple.ml *)
  (* clause.ml *)
  (* clauseSet.ml *)
  (* termSet.ml *)
  (* literalSet.ml *)
