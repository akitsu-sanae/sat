module Clause = Set.Make (struct
    type t = int
    let compare = compare
end)

let clauses =
    let ch = open_in "test.txt" in
    let lines = Std.input_list ch in
    List.map
        (fun line ->
            Clause.of_list @@ List.map int_of_string @@ Str.split (Str.regexp "[ \t]+") line)
        lines

(* make clauses simpler as given number is true *)
let rec subst cs given =
    let cs = List.filter (fun c -> not @@ Clause.mem given c) cs in
    List.map (fun c -> Clause.remove (-given) c) cs

exception Unsat

(* Clause list -> int list *)
let rec solve cs =
    let rec nyan c cs =
        if Clause.is_empty c then []
        else
            let e = Clause.choose c in
            let cs = subst cs e in
            if List.exists (fun c -> Clause.is_empty c) cs then
                raise Unsat
            else
                e :: (solve cs) in
    match cs with
    | [] -> []
    | c :: cs -> nyan c cs
;;

List.iter (fun e -> print_string @@ string_of_int e ^ ", ") @@ solve clauses;;
print_newline ()

