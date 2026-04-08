let pairs_of_ints s =
  List.map
    (fun x -> List.map int_of_string (String.split_on_char '-' x))
    (String.split_on_char ',' s)

let rec len n = if n < 10 then 1 else 1 + len (n / 10)
let rec pow n = match n with
    | 0 -> 1
    | m -> 10 * pow (n - 1)


let invalid_ids first last =
    let rec invalid_ids_aux id step last =
        if id > last
        then []
        else id :: invalid_ids_aux (id + step) step last in
    let l = len first in
    let p = pow (l / 2) in
    let a = (first / p) * p + (first / p) in
    let step = 1 * p + 1 in
    let start = if a >= first then a else  a + step in
    invalid_ids_aux start step last
    

let sum_invalid_ids intervals =
  match intervals with
  | [] -> 0
  | [ (first, last) ] -> List.fold_left (+) 0 (invalid_ids first last)
  | _ -> 0
