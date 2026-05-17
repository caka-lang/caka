open Alcotest
open Caka_parser
open Caka_parser.Utils

let cases =
  [
    test_case "[size]type" `Quick (fun () ->
        let code = "x : [10]int = []" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty =
                   Mocks.mk_types
                     (`TArray { ty = Mocks.mk_types (`TInt 32); size = Some 10 });
                 value = Mocks.mk_expr (`Array { items = []; ty = None });
               }));
    test_case "[]type" `Quick (fun () ->
        let code = "x : []int = []" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty =
                   Mocks.mk_types
                     (`TArray { ty = Mocks.mk_types (`TInt 32); size = None });
                 value = Mocks.mk_expr (`Array { items = []; ty = None });
               }));
  ]
