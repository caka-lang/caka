open Alcotest
open Caka_parser
open Caka_parser.Ast
open Caka_parser.Utils

let cases =
  [
    test_case "empty" `Quick (fun () ->
        let code = "" in
        check_raises "error on empty" (Errors.Error "main:1:0: syntax error")
          (fun () -> ignore (Lexer.resolve code)));
    test_case "simple" `Quick (fun () ->
        let code = "x := () void {}" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty =
                   Mocks.mk_types
                     (`TFn { params = []; ty = Mocks.t_void; name = None });
                 value =
                   Mocks.mk_expr
                   @@ `Fn
                        {
                          params = [];
                          ty = Some (Mocks.mk_types (`TVoid ()));
                          block = SBlock.make ();
                        };
               }));
    test_case "param" `Quick (fun () ->
        let code = "x := (a: int) void {}" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x, type is void, params are (a int)"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty =
                   Mocks.mk_types
                     (`TFn
                        {
                          params = [ Mocks.mk_types (`TInt 32) ];
                          ty = Mocks.t_void;
                          name = None;
                        });
                 value =
                   Mocks.mk_expr
                   @@ `Fn
                        {
                          params =
                            [ { name = "a"; ty = Mocks.mk_types (`TInt 32) } ];
                          ty = Some (Mocks.mk_types (`TVoid ()));
                          block = SBlock.make ();
                        };
               }));
    test_case "params" `Quick (fun () ->
        let code = "x := (a: int, b: int) void {}" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty =
                   Mocks.mk_types
                     (`TFn
                        {
                          params =
                            [
                              Mocks.mk_types (`TInt 32);
                              Mocks.mk_types (`TInt 32);
                            ];
                          ty = Mocks.t_void;
                          name = None;
                        });
                 value =
                   Mocks.mk_expr
                   @@ `Fn
                        {
                          params =
                            [
                              { name = "a"; ty = Mocks.mk_types (`TInt 32) };
                              { name = "b"; ty = Mocks.mk_types (`TInt 32) };
                            ];
                          ty = Some (Mocks.mk_types (`TVoid ()));
                          block = SBlock.make ();
                        };
               }));
    test_case "return" `Quick (fun () ->
        let code = "x := () int { return 10 }" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty =
                   Mocks.mk_types
                     (`TFn
                        {
                          params = [];
                          ty = Mocks.mk_types (`TInt 32);
                          name = None;
                        });
                 value =
                   Mocks.mk_expr
                   @@ `Fn
                        {
                          params = [];
                          ty = Some (Mocks.mk_types (`TInt 32));
                          block =
                            SBlock.make
                              ~statements:
                                [
                                  Mocks.mk_stmt
                                  @@ `Return (Mocks.mk_expr (`Int 10));
                                ]
                              ();
                        };
               }));
    test_case "return auto void" `Quick (fun () ->
        let code = "x := (a: int) {}" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty =
                   Mocks.mk_types
                     (`TFn
                        {
                          params = [ Mocks.mk_types (`TInt 32) ];
                          ty = Mocks.t_void;
                          name = None;
                        });
                 value =
                   Mocks.mk_expr
                     (`Fn
                        {
                          params =
                            [ { name = "a"; ty = Mocks.mk_types (`TInt 32) } ];
                          ty = None;
                          block = SBlock.make ();
                        });
               }));
    test_case "return auto type" `Quick (fun () ->
        let code = "x := () { return 10 }" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty =
                   Mocks.mk_types
                     (`TFn { params = []; ty = Mocks.t_void; name = None });
                 value =
                   Mocks.mk_expr
                   @@ `Fn
                        {
                          params = [];
                          ty = None;
                          block =
                            SBlock.make
                              ~statements:
                                [
                                  Mocks.mk_stmt
                                    (`Return (Mocks.mk_expr (`Int 10)));
                                ]
                              ();
                        };
               }));
    test_case "generics" `Quick (fun () ->
        let code = "a := <T>(x: T) {}" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "a";
                 ty =
                   Mocks.mk_types
                   @@ `TFn
                        {
                          name = None;
                          ty = Mocks.mk_types (`TVoid ());
                          params = [ Mocks.mk_types (`Named "T") ];
                        };
                 value =
                   Mocks.mk_expr
                   @@ `Fn
                        {
                          params =
                            [ { name = "x"; ty = Mocks.mk_types (`Named "T") } ];
                          ty = None;
                          block = SBlock.make ~statements:[] ();
                        };
                 mut = false;
               }));
  ]
