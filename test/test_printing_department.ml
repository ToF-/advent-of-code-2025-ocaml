open OUnit2

let tests =
    let sample = "../testdata/day04_sample.txt" in
    let accessible_rolls = Advent_of_code_2025.Printing_department.accessible_rolls in
    Printf.printf "day 04: printing department";
    "printing department" 
    >::: [
        ("accessible rolls for sample (A)" >:: (fun _ ->
            assert_equal ~printer:string_of_int 13 (accessible_rolls sample))) ;
    ]
