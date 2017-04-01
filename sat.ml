(*============================================================================
  Copyright (C) 2017 akitsu sanae
  https://github.com/akitsu-sanae/sat
  Distributed under the Boost Software License, Version 1.0. (See accompanying
  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
============================================================================*)

module Clause = Set.Make (struct
    type t = int
    let compare = compare
end)

module Problem = Set.Make (struct
    type t = Clause.t
    let compare = fun lhs rhs -> Clause.cardinal lhs - Clause.cardinal rhs
end)

let rec read_n_variables () =
    let line = read_line () in
    if line.[0] = 'p' then
        (* p cnf <n variables> <n clauses> *)
        int_of_string @@ List.nth (Str.split (Str.regexp "[ \t]+") line) 3
    else
        read_n_variables ()

let _ = read_n_variables ()

let read_data () =
    let lines = Std.input_list stdin in
    let line_to_clause line =
        let variable_strs = Str.split (Str.regexp "[ \t]+") line in
        let variables = List.rev_map int_of_string variable_strs in
        Clause.of_list @@ List.tl variables (* first element is 0 *)
    in
    Problem.of_list @@ List.rev_map line_to_clause lines

let (>>) f g = function x -> f(g(x))

let problem = read_data ()

let print_problem problem =
    let print_clause clause =
        print_endline @@ Clause.fold (fun i acc ->  acc ^ string_of_int i ^ ", ") clause ""
    in
    Problem.iter print_clause problem

let print_result result =
    print_endline @@ List.fold_left (fun acc i -> acc ^ string_of_int i ^ " " ) "" result ^ "0"


(* Clause  : Set of int    *)
(* Problem : Set of Clause *)

exception Unsat

let rec subst problem given =
    let problem = Problem.filter (fun clause -> not @@ Clause.mem given clause) problem in
    let problem = Problem.map (fun clause -> Clause.remove (-given) clause) problem in
    if Problem.for_all (not >> Clause.is_empty) problem then problem
    else raise Unsat

let rec solve problem result =
    let piv = Clause.choose @@ Problem.choose problem in
    try
        let problem = subst problem piv in
        if Problem.is_empty problem then piv::result
        else solve problem (piv::result)
    with Unsat ->
        let problem = subst problem (-piv) in
        if Problem.is_empty problem then (-piv)::result
        else solve problem ((-piv)::result)

let _ =
    try
        let result = solve problem [] in
        print_problem problem;
        print_endline "SAT";
        print_result @@ List.sort (fun l r -> abs l - abs r) result
    with Unsat ->
        print_endline "UNSAT"




