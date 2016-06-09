type fole =
| Exists of string*fole
| ForAll of string*fole
| Or of fole*fole
| And of fole*fole
| Not of fole
| Implies of fole*fole
| Predicate of string * string list


let rec implode glue = function
| [] -> ""
| [x] -> x
| x::l -> x^glue^(implode glue l)

let rec latex_of_fole = function
| Exists(v, e) -> "(\\exists "^v^" : "^(latex_of_fole e)^")"
| ForAll(v, e) -> "(\\forall "^v^" : "^(latex_of_fole e)^")"
| Or(p, q) -> "("^(latex_of_fole p)^" \\lor "^(latex_of_fole q)^")"
| And(p, q) -> "("^(latex_of_fole p)^" \\land "^(latex_of_fole q)^")"
| Implies(p, q) -> "("^(latex_of_fole p)^" \\implies "^(latex_of_fole q)^")"
| Not(p) -> "\\lnot "^(latex_of_fole p)
| Predicate(p, []) -> p
| Predicate(p, a) -> p^"("^(implode ", " a)^")"


let rec negate = function
| Exists(_, _) as p -> Not(p) (* ForAll(v, negate e) *)
| ForAll(v, e) -> Exists(v, negate e)
| Or(p, q) -> And(negate p, negate q)
| And(p, q) -> Or(negate p, negate q)
| Implies(p, q) -> And(p, negate q)
| Not(p) -> p
| Predicate(_, _) as p -> Not(p)

let rec remove_implies = function
| Exists(v, e) -> Exists(v, remove_implies e)
| ForAll(v, e) -> ForAll(v, remove_implies e)
| Or(p, q) -> Or(remove_implies p, remove_implies q)
| And(p, q) -> And(remove_implies p, remove_implies q)
| Implies(p, q) -> Or(negate p, remove_implies q)
| Not(p) -> Not(remove_implies p)
| Predicate(_, _) as p -> p;;


type clause = Clause of fole * fole (* Predicate, expr *)
let print_clauses clauses = let f = (function Clause(p, b) ->
    Printf.printf "%s <- %s\n" (latex_of_fole p) (latex_of_fole b)) in
    ignore (List.map f clauses)
let next = function (* TODO increment the trailing number if any *)
| Clause(Predicate(x, _), _) :: _ -> x^"'"
| _::_ -> "ERROR" (* FIXME *)
| [] -> "Tmp"

module SS = Set.Make(String)
let order s = List.sort (fun x y -> if x<y then -1 else 1) (SS.elements s)
let rec datalog_of_fole clauses = function
(* returned: expression, updated clauses, inner parameters *)
| Predicate(_, a) as p -> p, clauses, SS.of_list a
| ForAll(v, e) -> datalog_of_fole clauses (Not(Exists(v, Not(e))))
| Implies(p, q) -> datalog_of_fole clauses (Or(Not(p), q))
| Not(And(p, q)) -> datalog_of_fole clauses (Or(Not(p), Not(q)))
| Not(Or(p, q)) -> datalog_of_fole clauses (And(Not(p), Not(q)))
| Not(p) ->
    let b, clauses, inner_fr = datalog_of_fole clauses p in
    Not(b), clauses, inner_fr
| Exists(v, i) ->
    let b, clauses, inner_fr = datalog_of_fole clauses i in
    let n = next clauses in
    let inner_fr = (SS.diff inner_fr (SS.singleton v)) in
    let p = Predicate(n, order inner_fr) in
    p, Clause(p, b)::clauses, inner_fr
| Or(p, q) -> (* TODO create separate clauses *)
    let p2, clauses, inner_fr = datalog_of_fole clauses p in
    let q2, clauses, inner_fr2 = datalog_of_fole clauses q in
    Or(p2, q2), clauses, SS.union inner_fr inner_fr2
| And(p, q) ->
    let p2, clauses, inner_fr = datalog_of_fole clauses p in
    let q2, clauses, inner_fr2 = datalog_of_fole clauses q in
    And(p2, q2), clauses, SS.union inner_fr inner_fr2;;


let test formula =
    let b, c, _ = datalog_of_fole [] formula in
    print_endline (latex_of_fole formula);
    print_newline ();
    print_clauses c;
    print_endline (latex_of_fole b);;

let trivial = And(Predicate("z", []), Exists("x", And(
    Predicate("a", ["x"]),
    Exists("y", Predicate("b", ["x"; "y"]))))) in
    test trivial;
