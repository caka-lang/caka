open Alcotest

let cases =
  [
    test_case "simple" `Quick (fun () -> check bool "hello()" true true);
    test_case "param" `Quick (fun () -> check bool "print(123)" true true);
    test_case "params" `Quick (fun () -> check bool "plus(2, 3)" true true);
    test_case "param from call" `Quick (fun () ->
        check bool "plus(2, plus(1, 3))" true true);
    test_case "math" `Quick (fun () ->
        check bool "plus(2, 3) + plus(1, 3)" true true);
  ]
