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

let sample = ForAll("x", Implies(
    Predicate("frequente", ["x"; "y"]),
    Exists("z", And(
        Predicate("sert", ["y"; "z"]),
        Predicate("aime", ["x"; "z"])))))


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
let rec datalog_of_fole current clauses = function
(* current contains the free variables at the current level *)
(* returned: expression, updated clauses, inner parameters *)
| Predicate(_, a) as p -> p, clauses, SS.of_list a
| ForAll(v, e) -> datalog_of_fole SS.empty clauses (Not(Exists(v, Not(e))))
| Implies(p, q) -> datalog_of_fole SS.empty clauses (Or(Not(p), q))
| Not(p) ->
    let b, clauses, inner_fr = datalog_of_fole SS.empty clauses p in
    Not(b), clauses, inner_fr
| Exists(v, i) ->
    let b, clauses, inner_fr = datalog_of_fole SS.empty clauses i in
    let params = SS.remove v inner_fr in
    let p = Predicate(next clauses, order params) in
    p, Clause(p, b)::clauses, params
| Or(p, q) -> (* TODO create new clauses *)
    (* TODO implement this *)
    p, [], SS.empty
| And(p, q) ->
    (* TODO implement this *)
    p, [], SS.empty;;

let trivial = ForAll("x", Predicate("lt", ["x"; "x"])) in
let b, c, _ = datalog_of_fole SS.empty [] trivial in
print_clauses c;
print_endline (latex_of_fole b)
