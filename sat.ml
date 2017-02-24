module Truth = Set.Make (struct
    type t = int
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

let read_data () = begin
    let lines = Std.input_list stdin in
    let line_to_variables line =
        let variable_strs = Str.split (Str.regexp "[ \t]") line in
        let variables = List.rev_map int_of_string variable_strs in
        List.tl variables (* first element is 0 *)
    in
    List.rev_map line_to_variables lines
end

let clauses = read_data ()

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

try
    let answer = solve clauses Truth.empty in
    print_endline "s SATISFIABLE";
    print_string "v ";
    Truth.iter (fun n -> print_string @@ string_of_int n ^ " ") answer;
    print_endline " 0"
with Unsat ->
    print_endline "s UNSATISFIABLE";
    exit 20
;;

