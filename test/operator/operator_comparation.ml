open Alcotest
open Caka_parser
open Caka_parser.Utils
open Caka_parser.Ast

let cases =
  [
    test_case "EQ" `Quick (fun () ->
        let code = "x := 1 == 1" in
        let m = code |> Resolver.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (mk_stmt Lexing.dummy_pos
             (`Let
                {
                  name = "x";
                  ty = None;
                  value =
                    Mocks.mk_expr
                    @@ `Eq
                         {
                           l = mk_expr Lexing.dummy_pos (`Int 1);
                           r = mk_expr Lexing.dummy_pos (`Int 1);
                         };
                })));
    test_case "NE" `Quick (fun () ->
        let code = "x := 1 != 1" in
        let m = code |> Resolver.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty = None;
                 value =
                   Mocks.mk_expr
                   @@ `Ne
                        {
                          l = mk_expr Lexing.dummy_pos (`Int 1);
                          r = mk_expr Lexing.dummy_pos (`Int 1);
                        };
               }));
    test_case "LT" `Quick (fun () ->
        let code = "x := 1 < 1" in
        let m = code |> Resolver.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (mk_stmt Lexing.dummy_pos
             (`Let
                {
                  name = "x";
                  ty = None;
                  value =
                    Mocks.mk_expr
                    @@ `Lt
                         {
                           l = mk_expr Lexing.dummy_pos (`Int 1);
                           r = mk_expr Lexing.dummy_pos (`Int 1);
                         };
                })));
    test_case "LTE" `Quick (fun () ->
        let code = "x := 1 <= 1" in
        let m = code |> Resolver.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (mk_stmt Lexing.dummy_pos
             (`Let
                {
                  name = "x";
                  ty = None;
                  value =
                    Mocks.mk_expr
                    @@ `Lte
                         {
                           l = mk_expr Lexing.dummy_pos (`Int 1);
                           r = mk_expr Lexing.dummy_pos (`Int 1);
                         };
                })));
    test_case "GT" `Quick (fun () ->
        let code = "x := 1 > 1" in
        let m = code |> Resolver.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (mk_stmt Lexing.dummy_pos
             (`Let
                {
                  name = "x";
                  ty = None;
                  value =
                    Mocks.mk_expr
                      (`Gt
                         {
                           l = mk_expr Lexing.dummy_pos (`Int 1);
                           r = mk_expr Lexing.dummy_pos (`Int 1);
                         });
                })));
    test_case "GTE" `Quick (fun () ->
        let code = "x := 1 >= 1" in
        let m = code |> Resolver.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (mk_stmt Lexing.dummy_pos
             (`Let
                {
                  name = "x";
                  ty = None;
                  value =
                    Mocks.mk_expr
                    @@ `Gte
                         {
                           l = mk_expr Lexing.dummy_pos (`Int 1);
                           r = mk_expr Lexing.dummy_pos (`Int 1);
                         };
                })));
  ]
