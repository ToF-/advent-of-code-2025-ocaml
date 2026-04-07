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
