let read_line file_name =
  String.trim (In_channel.with_open_bin file_name In_channel.input_all)

let pairs_of_ints s =
  List.map
    (fun s ->
      let l = List.map int_of_string (String.split_on_char '-' s) in
      match l with [ a; b ] -> (a, b) | _ -> invalid_arg "incorrect interval")
    (String.split_on_char ',' s)

let rec len n = if n < 10 then 1 else 1 + len (n / 10)
let rec pow n = match n with 0 -> 1 | m -> 10 * pow (n - 1)

let rec sum_ids_a id_min id_max =
  let rec sum_ids_step curr step limit =
    if curr > id_max || curr > limit then 0
    else curr + sum_ids_step (curr + step) step limit
  in
  let l = len id_min in
  let m = len id_max in
  let p = pow (l / 2) in
  let lim = pow l - 1 in
  if l mod 2 > 0 then sum_ids_a (lim + 1) id_max
  else
    let first_id = (id_min / p * p) + (id_min / p) in
    let step = p + 1 in
    let start = if first_id >= id_min then first_id else first_id + step in
    if start > id_max then 0
    else
      sum_ids_step start step lim
      + if l < m then sum_ids_a (lim + 1) id_max else 0

(*
    rept 5 10 7 → 5555555 
    rept 37 100 2 → 3737
    rept 54 100 4 → 54545454
    rept 435 1000 2 → 435435
    rept 4807 10000 3 → 480748074807
*)
let rec rept pat factor count =
  if count == 1 then pat else pat + (factor * rept pat factor (count - 1))

let rec fstd n factor = if n < factor then n else fstd (n / factor) factor

module IntSet = Set.Make (Int)

let rec sum_ids_b id_min id_max =
  Printf.printf "sum_ids_b %d %d\n" id_min id_max;
  let ids = IntSet.empty in
  let l = len id_min in
  let m = len id_max in
  let lim = pow l - 1 in
  let rec collect_ids curr step limit set =
    Printf.printf "l:%d\tcurr:%d step:%d limit:%d\n" l curr step limit;
    if curr > id_max || curr > limit then set
    else collect_ids (curr + step) step limit (IntSet.add curr set)
  in
  let collect_pat_ids p l lp ids =
    let pat = fstd id_min (pow p) in
    let i = rept pat (pow p) lp in
    let step = rept 1 (pow p) lp in
    let init = if i >= id_min then i else i + step in
    Printf.printf "pat:%d i:%d init:%d step:%d\n" pat i init step;
    collect_ids init step lim ids
  in
  if l < m then sum_ids_b id_min lim + sum_ids_b (lim + 1) id_max
  else
    List.fold_left ( + ) 0
      (IntSet.to_list
         (match l with
         | 2 -> collect_pat_ids 1 l 2 ids
         | 4 -> collect_pat_ids 2 l 2 ids
         | 6 ->
             let set1 = collect_pat_ids 3 l 2 ids in
             let set2 = collect_pat_ids 2 l 3 set1 in
             IntSet.union set1 set2
         | 8 ->
             let set1 = collect_pat_ids 4 l 2 ids in
             let set2 = collect_pat_ids 2 l 4 set1 in
             IntSet.union set1 set2
         | 9 ->
             let set1 = collect_pat_ids 1 l 9 ids in
             let set2 = collect_pat_ids 3 l 3 set1 in
             IntSet.union set1 set2
         | 10 ->
             let set1 = collect_pat_ids 5 l 2 ids in
             let set2 = collect_pat_ids 2 l 5 set1 in
             IntSet.union set1 set2
            
         | n when n mod 2 > 0 -> collect_pat_ids 1 l l ids
         | _ -> IntSet.empty))

let rec sum_invalid_ids intervals f_acc =
  match intervals with
  | [] -> 0
  | (id_min, id_max) :: rest -> f_acc id_min id_max + sum_invalid_ids rest f_acc

let sum_invalid_ids_from_file file_name f_acc =
    Printf.printf "sum_invalid_ids\n" ;
  let line = read_line file_name in
  let intervals = pairs_of_ints line in
  sum_invalid_ids intervals f_acc
