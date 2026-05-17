open Alcotest
open Caka_parser
open Caka_parser.Ast
open Caka_parser.Utils

let cases =
  [
    test_case "plus" `Quick (fun () ->
        let code = "x := 1 + 1" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
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
                          r = Mocks.mk_expr (`Int 1);
                        };
               }));
    test_case "minus" `Quick (fun () ->
        let code = "x := 1 - 1" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TInt 32);
                 value =
                   Mocks.mk_expr
                   @@ `Sub
                        {
                          l = Mocks.mk_expr (`Int 1);
                          r = Mocks.mk_expr (`Int 1);
                        };
               }));
    test_case "div" `Quick (fun () ->
        let code = "x := 1 / 2" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TFloat 64);
                 value =
                   Mocks.mk_expr
                   @@ `Div
                        {
                          l = Mocks.mk_expr (`Int 1);
                          r = Mocks.mk_expr (`Int 2);
                        };
               }));
    test_case "mul" `Quick (fun () ->
        let code = "x := 1 * 1" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TInt 32);
                 value =
                   Mocks.mk_expr
                   @@ `Mul
                        {
                          l = Mocks.mk_expr (`Int 1);
                          r = Mocks.mk_expr (`Int 1);
                        };
               }));
    test_case "modulus" `Quick (fun () ->
        let code = "x := 1 % 1" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TInt 32);
                 value =
                   Mocks.mk_expr
                   @@ `Mod
                        {
                          l = Mocks.mk_expr (`Int 1);
                          r = Mocks.mk_expr (`Int 1);
                        };
               }));
    test_case "pow" `Quick (fun () ->
        let code = "x := 1 ^ 1" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TInt 32);
                 value =
                   Mocks.mk_expr
                   @@ `Pow
                        {
                          l = Mocks.mk_expr (`Int 1);
                          r = Mocks.mk_expr (`Int 1);
                        };
               }));
    test_case "nested plus minus" `Quick (fun () ->
        let code = "x := (1 + 2) - (1 + 2)" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TInt 32);
                 value =
                   Mocks.mk_expr
                   @@ `Sub
                        {
                          l =
                            Mocks.mk_expr
                            @@ `Add
                                 {
                                   l = Mocks.mk_expr (`Int 1);
                                   r = Mocks.mk_expr (`Int 2);
                                 };
                          r =
                            Mocks.mk_expr
                            @@ `Add
                                 {
                                   l = Mocks.mk_expr (`Int 1);
                                   r = Mocks.mk_expr (`Int 2);
                                 };
                        };
               }));
    test_case "nested div mul mod pow" `Quick (fun () ->
        let code = "x := (1 % 2) * (1 % 2)" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = Mocks.mk_types (`TInt 32);
                 value =
                   Mocks.mk_expr
                   @@ `Mul
                        {
                          l =
                            Mocks.mk_expr
                            @@ `Mod
                                 {
                                   l = Mocks.mk_expr (`Int 1);
                                   r = Mocks.mk_expr (`Int 2);
                                 };
                          r =
                            Mocks.mk_expr
                            @@ `Mod
                                 {
                                   l = Mocks.mk_expr (`Int 1);
                                   r = Mocks.mk_expr (`Int 2);
                                 };
                        };
               }));
    test_case "nested plus minus div mul mod pow" `Quick (fun () ->
        let code = "x := (1 % 2) + (4 - 2) * (1 % 2)" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
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
                          l =
                            Mocks.mk_expr
                            @@ `Mod
                                 {
                                   l = Mocks.mk_expr (`Int 1);
                                   r = Mocks.mk_expr (`Int 2);
                                 };
                          r =
                            Mocks.mk_expr
                            @@ `Mul
                                 {
                                   l =
                                     Mocks.mk_expr
                                     @@ `Sub
                                          {
                                            l = Mocks.mk_expr (`Int 4);
                                            r = Mocks.mk_expr (`Int 2);
                                          };
                                   r =
                                     Mocks.mk_expr
                                     @@ `Mod
                                          {
                                            l = Mocks.mk_expr (`Int 1);
                                            r = Mocks.mk_expr (`Int 2);
                                          };
                                 };
                        };
               }));
  ]
