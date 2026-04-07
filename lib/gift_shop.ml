let pairs_of_ints s =
  List.map
    (fun x -> List.map int_of_string (String.split_on_char '-' x))
    (String.split_on_char ',' s)

let rec len n = if n < 10 then 1 else 1 + len (n / 10)
let power_of_ten n = int_of_float (10.0 ** float_of_int n)

let half n =
  let l = len n in
  n / power_of_ten (l / 2)

let is_invalid id =
  let l = String.length id in
  match l with
  | 0 -> false
  | len when len mod 2 <> 0 -> false
  | _ ->
      let half_len = l / 2 in
      let half = String.sub id 0 half_len in
      String.equal id (half ^ half)

let sum_invalid_ids file_name = 0
