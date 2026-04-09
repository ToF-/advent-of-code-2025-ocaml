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

let rec sum_ids id_min id_max =

    let rec sum_ids_step curr step limit =
        if curr > id_max || curr > limit
        then 0
        else curr + sum_ids_step (curr + step) step limit
    in
    let l = len id_min in
    let m = len id_max in
    let p = pow (l / 2) in
    let lim = pow l - 1 in
    if l mod 2 > 0 then sum_ids (lim + 1) id_max
    else
        let first_id = (id_min / p * p) + (id_min / p) in
        let step = p + 1 in
        let start = if first_id >= id_min then first_id else first_id + step in
        if start > id_max then 0
        else
            sum_ids_step start step lim
            + if l < m then sum_ids (lim + 1) id_max else 0


let rec sum_invalid_ids intervals =
  match intervals with
  | [] -> 0
  | (id_min, id_max) :: rest -> sum_ids id_min id_max + sum_invalid_ids rest

let sum_invalid_ids_from_file file_name =
  let line = read_line file_name in
  let intervals = pairs_of_ints line in
  sum_invalid_ids intervals
