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
                 mut = false;
                 ty = Mocks.mk_types (`TInt 32);
                 value = Mocks.mk_expr (`Int 0);
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
                 mut = false;
                 ty = Mocks.mk_types (`TInt 32);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "let mut int" `Quick (fun () ->
        let code = "x: mut int = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x, type is mut int, value is 0"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = true;
                 ty = Mocks.mk_types (`TInt 32);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "let mut auto" `Quick (fun () ->
        let code = "x: mut = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x, type is mut None, value is 0"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = true;
                 ty = Mocks.mk_types (`TInt 32);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "let mut default" `Quick (fun () ->
        let code = "x: mut int" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x, type is mut int, value is None"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = true;
                 ty = Mocks.mk_types (`TInt 32);
                 value = Mocks.mk_expr (`Int 0);
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
                   mut = false;
                   ty = Mocks.mk_types (`TBool ());
                   value = Mocks.mk_expr (`Bool true);
                 };
            Mocks.mk_stmt
            @@ `Let
                 {
                   mut = false;
                   name = "y";
                   ty = Mocks.mk_types (`TBool ());
                   value = Mocks.mk_expr (`Bool false);
                 };
          ]);
  ]
