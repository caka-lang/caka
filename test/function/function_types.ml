open Alcotest
open Caka_parser
open Caka_parser.Ast
open Caka_parser.Utils

let cases =
  [
    test_case "without body" `Quick (fun () ->
        let code = "x : (int) bool = nil" in
        let m = code |> Resolver.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 ty =
                   Some
                     (Mocks.mk_types
                     @@ `TFn
                          {
                            name = None;
                            params = [ Mocks.mk_types (`TInt 32) ];
                            ty = Mocks.mk_types (`TBool ());
                          });
                 value = Mocks.mk_expr (`Nil ());
               }));
  ]
