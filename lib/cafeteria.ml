open Utils

let acquire lines =
  let parse_interval s =
    match String.split_on_char '-' s with
    | [ s1; s2 ] -> (int_of_string s1, int_of_string s2)
    | _ -> invalid_arg "incorrect interval"
  in
  let ss1, ss2 = List.partition (fun s -> String.contains s '-') lines in
  ( List.map (fun s -> parse_interval s) ss1,
    List.map (fun s -> int_of_string s) ss2 )

let rec is_fresh ingredient intervals =
  let within x (a, b) = x >= a && x <= b in
  match intervals with
  | [] -> false
  | interval :: rest -> within ingredient interval || is_fresh ingredient rest

let rec scan_fold f list =
  match list with
| [] -> []
| (elem::rest) -> List.append (List.fold_left (fun acc item -> List.cons (f elem item) acc) [] rest) (scan_fold f rest)



let rec intersect (a_start, a_end) (b_start, b_end) =
    if b_start < a_start
    then intersect (b_start, b_end) (a_start, a_end)
    else (b_start, min a_end b_end)

let size (a,b) =
    if a > b then 0 else (b - a) + 1
    
let merge_intervals (a_start, a_end) (b_start, b_end) =
    if b_start > a_end then None
    else Some (a_start, max a_end b_end)

let rec merge intervals = 
    match intervals with
    | [] -> []
    | [interval] -> [interval]
    | (interval_a :: interval_b :: rest) -> match merge_intervals interval_a interval_b with
        | Some result -> merge (result :: rest)
        | None -> interval_a :: merge (interval_b :: rest)

let compare (a_start,a_end) (b_start,b_end) = a_start - b_start

let sort intervals = List.sort compare intervals

let fresh_ingredients file_name option_b =
  let lines = Utils.read_lines file_name in
  let intervals, ingredients = acquire lines in
  if option_b then 
      let sorted = sort intervals in
      List.fold_left (fun acc (a,b) -> acc + (b - a) + 1) 0 (merge sorted)
  else
    List.fold_left
      (fun acc ingredient ->
        acc + if is_fresh ingredient intervals then 1 else 0)
      0 ingredients
