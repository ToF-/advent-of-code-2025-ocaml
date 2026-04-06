open OUnit2

let tests = 
    "all tests" >::: [
        "secret entrance with sample a" >::
            (fun _ -> assert_equal ~printer:string_of_int 3 (Secret_entrance.actual_password_a "../testdata/day01_sample.txt")) ;
        "secret entrance with input a" >::
            (fun _ -> assert_equal ~printer:string_of_int 964 (Secret_entrance.actual_password_a "../testdata/day01_input.txt")) ;
        "secret entrance with sample b" >::
            (fun _ -> assert_equal ~printer:string_of_int 3 (Secret_entrance.actual_password_b "../testdata/day01_sample.txt")) ;
    ]

let _ =
    run_test_tt_main tests
