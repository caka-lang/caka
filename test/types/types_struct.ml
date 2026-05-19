open Alcotest
open Caka_parser
open Caka_parser.Utils

let cases =
  [
    test_case "empty" `Quick (fun () ->
        let code = "struct Point {}" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt @@ `Struct { name = "Point"; fields = [] }));
    test_case "one field" `Quick (fun () ->
        let code = "struct Point { x: int }" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Struct
               {
                 name = "Point";
                 fields = [ { name = "x"; ty = Mocks.mk_types (`TInt 32) } ];
               }));
    test_case "some fields" `Quick (fun () ->
        let code = "struct Point { x: int, y: int }" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Struct
               {
                 name = "Point";
                 fields =
                   [
                     { name = "x"; ty = Mocks.mk_types (`TInt 32) };
                     { name = "y"; ty = Mocks.mk_types (`TInt 32) };
                   ];
               }));
  ]
