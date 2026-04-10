
let read_line file_name =
  String.trim (In_channel.with_open_bin file_name In_channel.input_all)

let read_lines file_name =
  let lines =
    String.split_on_char '\n'
      (In_channel.with_open_bin file_name In_channel.input_all)
  in
  List.filter (fun s -> s <> "") lines

