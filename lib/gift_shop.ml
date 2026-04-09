let read_line file_name =
  String.trim (In_channel.with_open_bin file_name In_channel.input_all)

let pairs_of_ints s =
  List.map
    (fun s ->
      let l = List.map int_of_string (String.split_on_char '-' s) in
      match l with [ a; b ] -> (a, b) | _ -> invalid_arg "incorrect interval")
    (String.split_on_char ',' s)

let rec number_length n = if n < 10 then 1 else 1 + number_length (n / 10)
let rec power_10 n = match n with 0 -> 1 | m -> 10 * power_10 (n - 1)

let rec repeat pattern factor count =
  if count == 1 then pattern
  else pattern + (factor * repeat pattern factor (count - 1))

let first_digits n length =
  let rec first_digits_under n limit =
    if n < limit then n else first_digits_under (n / limit) limit
  in
  first_digits_under n (power_10 length)

module IntSet = Set.Make (Int)

let rec sum_ids id_min id_max option_b =
  let ids = IntSet.empty in
  let length = number_length id_min in
  let id_max_length = number_length id_max in
  let limit = power_10 length - 1 in

  let rec collect_ids current step set =
    if current < 10 || current > id_max || current > limit then set
    else collect_ids (current + step) step (IntSet.add current set)
  in

  let collect_pattern_ids pattern_length nb_repeat ids =
    let pattern = first_digits id_min pattern_length in
    let initial = repeat pattern (power_10 pattern_length) nb_repeat in
    let step = repeat 1 (power_10 pattern_length) nb_repeat in
    let first = if initial >= id_min then initial else initial + step in
    collect_ids first step ids
  in
  if length < id_max_length then
    (sum_ids id_min limit option_b) + (sum_ids (limit + 1) id_max option_b)
  else
    List.fold_left ( + ) 0
      (IntSet.to_list
         (match length with
         | 2 -> collect_pattern_ids 1 2 ids
         | 4 -> collect_pattern_ids 2 2 ids
         | 6 -> collect_pattern_ids 3 2 (if option_b then collect_pattern_ids 2 3 ids else ids)
         | 8 -> collect_pattern_ids 4 2 (if option_b then collect_pattern_ids 2 4 ids else ids)
         | 9 -> if option_b then collect_pattern_ids 1 9 (collect_pattern_ids 3 3 ids) else ids
         | 10 -> collect_pattern_ids 5 2 (if option_b then collect_pattern_ids 2 5 ids else ids)
         | n when n mod 2 > 0 -> if option_b then collect_pattern_ids 1 length ids else ids
         | _ -> IntSet.empty))

let rec sum_invalid_ids intervals option_b =
  match intervals with
  | [] -> 0
  | (id_min, id_max) :: rest -> (sum_ids id_min id_max option_b) + sum_invalid_ids rest option_b

let sum_invalid_ids_from_file file_name option_b =
  let line = read_line file_name in
  let intervals = pairs_of_ints line in
  sum_invalid_ids intervals option_b
