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
    repeat 5 10 7 → 5555555 
    repeat 37 100 2 → 3737
    repeat 54 100 4 → 54545454
    repeat 435 1000 2 → 435435
    repeat 4807 10000 3 → 480748074807
*)
let rec repeat pattern factor count =
  if count == 1 then pattern
  else pattern + (factor * repeat pattern factor (count - 1))

let first_digits n length =
  let rec first_digits_under n limit =
    if n < limit then n else first_digits_under (n / limit) limit
  in
  first_digits_under n (pow length)

module IntSet = Set.Make (Int)

let rec sum_ids_b id_min id_max =
  let ids = IntSet.empty in
  let length = len id_min in
  let id_max_length = len id_max in
  let limit = pow length - 1 in

  let rec collect_ids current step set =
    if current < 10 || current > id_max || current > limit then set
    else collect_ids (current + step) step (IntSet.add current set)
  in
  let collect_pattern_ids pattern_length length lp ids =
    let pattern = first_digits id_min pattern_length in
    let initial = repeat pattern (pow pattern_length) lp in
    let step = repeat 1 (pow pattern_length) lp in
    let first = if initial >= id_min then initial else initial + step in
    collect_ids first step ids
  in
  if length < id_max_length then
    sum_ids_b id_min limit + sum_ids_b (limit + 1) id_max
  else
    List.fold_left ( + ) 0
      (IntSet.to_list
         (match length with
         | 2 -> collect_pattern_ids 1 length 2 ids
         | 4 -> collect_pattern_ids 2 length 2 ids
         | 6 ->
             collect_pattern_ids 3 length 2 (collect_pattern_ids 2 length 3 ids)
         | 8 ->
             collect_pattern_ids 4 length 2 (collect_pattern_ids 2 length 4 ids)
         | 9 ->
             collect_pattern_ids 1 length 9 (collect_pattern_ids 3 length 3 ids)
         | 10 ->
             collect_pattern_ids 5 length 2 (collect_pattern_ids 2 length 5 ids)
         | n when n mod 2 > 0 -> collect_pattern_ids 1 length length ids
         | _ -> IntSet.empty))

let rec sum_invalid_ids intervals f_acc =
  match intervals with
  | [] -> 0
  | (id_min, id_max) :: rest -> f_acc id_min id_max + sum_invalid_ids rest f_acc

let sum_invalid_ids_from_file file_name f_acc =
  let line = read_line file_name in
  let intervals = pairs_of_ints line in
  sum_invalid_ids intervals f_acc
