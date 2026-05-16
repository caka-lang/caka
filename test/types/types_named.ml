open Alcotest
open Caka_parser
open Caka_parser.Utils

let cases =
  [
    test_case "primitive" `Quick (fun () ->
        let code = "type number int" in
        let m = code |> Resolver.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Type { name = "number"; ty = Mocks.mk_types (`TInt 32) }));
    test_case "array" `Quick (fun () ->
        let code = "type numbers []int" in
        let m = code |> Resolver.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Type
               {
                 name = "numbers";
                 ty =
                   Mocks.mk_types
                     (`TArray { size = None; ty = Mocks.mk_types (`TInt 32) });
               }));
  ]
