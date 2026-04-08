let pairs_of_ints s =
  List.map
    (fun x -> List.map int_of_string (String.split_on_char '-' x))
    (String.split_on_char ',' s)

let rec len n = if n < 10 then 1 else 1 + len (n / 10)
let rec pow n = match n with 0 -> 1 | m -> 10 * pow (n - 1)

let rec invalid_ids first last =
  let rec invalid_ids_aux id step limit last =
    Printf.printf "id:%d step:%d limit:%d last:%d\n" id step limit last;
    if id > last || id > limit then []
    else id :: invalid_ids_aux (id + step) step limit last
  in
  let l = len first in
  let m = len last in
  let p = pow (l / 2) in
  let a = (first / p * p) + (first / p) in
  let step = p + 1 in
  let limit = pow l - 1 in
  let start = if a >= first then a else a + step in
  if start > last then []
  else
    List.append
      (if l mod 2 == 0 then invalid_ids_aux start step limit last
       else invalid_ids (limit + 1) last)
      (if l < m then invalid_ids (limit + 1) last else [])

let sum_invalid_ids intervals =
  match intervals with
  | [] -> 0
  | [ (first, last) ] -> List.fold_left ( + ) 0 (invalid_ids first last)
  | _ -> 0
