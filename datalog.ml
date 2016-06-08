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
let _ =
    print_endline ("expression:\t"^latex_of_fole sample);
    print_endline ("negation:\t"^(latex_of_fole (negate sample)));
    print_endline ("no_implies:\t"^(latex_of_fole (remove_implies sample)))
