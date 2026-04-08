open OUnit2

let _ =
  List.map run_test_tt_main
    [ Test_secret_entrance.tests ; Test_gift_shop.tests ; ]
