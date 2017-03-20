module Clause = Set.Make (struct
    type t = int
    let compare = compare
end)

module Clauses = Set.Make( struct
    type t = Clause
    let compare = compare
end)

let rec read_n_variables () =
    let line = read_line () in
    if line.[0] = 'p' then
        (* p cnf <n variables> <n clauses> *)
        int_of_string @@ List.nth (Str.split (Str.regexp "[ \t]+") line) 3
    else
        read_n_variables ()

let _ = read_n_variables ()

let read_data ()  : Clause.t list =
    let lines = Std.input_list stdin in
    let line_to_variables line =
        let variable_strs = Str.split (Str.regexp "[ \t]+") line in
        let variables = List.rev_map int_of_string variable_strs in
        Clause.of_list @@ List.tl variables (* first element is 0 *)
    in
    List.rev_map line_to_variables lines

let (>>) f g = function x -> f(g(x))

let clauses = read_data ()

let print_clauses clauses =
    let print_clause clause =
        print_endline @@ Clause.fold (fun i acc ->  acc ^ string_of_int i ^ ", ") clause ""
    in
    List.iter print_clause clauses

let print_result result =
    print_endline @@ List.fold_left (fun acc i -> acc ^ string_of_int i ^ " " ) "" result ^ "0"

exception Unsat

let rec subst (clauses: Clause.t list) given =
    let clauses = List.filter (fun clause -> not @@ Clause.mem given clause) clauses in
    let clauses = List.rev_map (fun clause -> Clause.remove (-given) clause) clauses in
    if List.for_all (not >> Clause.is_empty) clauses then clauses
    else raise Unsat

let rec solve (clauses: Clause.t list) result : int list =
    let piv = Clause.choose @@ List.hd clauses in
    try
        let clauses = subst clauses piv in
        if List.length clauses = 0 then piv::result
        else solve clauses (piv::result)
    with Unsat ->
        let clauses = subst clauses (-piv) in
        if List.length clauses = 0 then (-piv)::result
        else solve clauses ((-piv)::result)

let _ =
    try
        let result = solve clauses [] in
        print_endline "SAT";
        print_result @@ List.sort (fun l r -> abs l - abs r) result
    with Unsat ->
        print_endline "UNSAT"




