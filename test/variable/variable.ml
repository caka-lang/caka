open Alcotest
open Caka_parser
open Caka_parser.Utils

let cases =
  [
    test_case "let int" `Quick (fun () ->
        let code = "x : int = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x, type is int, value is 0"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TInt 32);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "let uint" `Quick (fun () ->
        let code = "x : uint = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x, type is uint, value is 0"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TUint 32);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "let float" `Quick (fun () ->
        let code = "x : float = 0.0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x, type is float, value is 0.0"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TFloat 64);
                 value = Mocks.mk_expr (`Float 0.0);
               }));
    test_case "bool" `Quick (fun () ->
        let code = "x : bool = false" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x, type is bool, value is false"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TBool ());
                 value = Mocks.mk_expr (`Bool false);
               }));
    test_case "let auto" `Quick (fun () ->
        let code = "x := 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x, type is None, value is 0"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TInt 32);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "let and math" `Quick (fun () ->
        let code = "x := 1 + 2" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x, type is None, value is (add 1 2)"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TInt 32);
                 value =
                   Mocks.mk_expr
                   @@ `Add
                        {
                          l = Mocks.mk_expr (`Int 1);
                          r = Mocks.mk_expr (`Int 2);
                        };
               }));
    test_case "let true or false" `Quick (fun () ->
        let code = "x := true\ny := false" in
        let m = code |> Lexer.resolve |> List.hd in
        check (list Mocks.stmt_testable)
          "name is x, type is bool, value true\n\n\
          \          name is y, type is bool, value false"
          m.block.statements
          [
            Mocks.mk_stmt
            @@ `Let
                 {
                   name = "x";
                   ty = Mocks.mk_types (`TBool ());
                   value = Mocks.mk_expr (`Bool true);
                 };
            Mocks.mk_stmt
            @@ `Let
                 {
                   name = "y";
                   ty = Mocks.mk_types (`TBool ());
                   value = Mocks.mk_expr (`Bool false);
                 };
          ]);
  ]
