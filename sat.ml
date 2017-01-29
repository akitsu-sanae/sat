module Truth = Set.Make (struct
    type t = int
    let compare = compare
end)

let ch = open_in "test.txt";;
let lines = Std.input_list ch;;

let clauses = List.map
    (fun line ->
        List.map int_of_string @@ Str.split (Str.regexp "[ \t]+") line)
    lines

exception Unsat

let rec solve clauses truth =
    let rec solve_impl clause truth new_truth =
        match clause with
        | [] ->
                if Truth.cardinal new_truth = 0 then raise Unsat
                else new_truth
        | n :: clause -> begin
            if Truth.mem n truth || Truth.mem (-n) new_truth then
                Truth.empty (* none *)
            else
                let is_known = Truth.mem (-n) truth || Truth.mem n new_truth in
                let new_truth = if is_known then new_truth else Truth.add n new_truth in
                solve_impl clause truth new_truth end in
    match clauses with
    | [] -> truth
    | clause :: clauses ->
            let new_truth = solve_impl clause truth Truth.empty in
            solve clauses @@ Truth.union truth new_truth;;

let answer = solve clauses Truth.empty;;
Truth.iter (fun n -> print_string @@ string_of_int n ^ ", ") answer;;
print_newline ()

