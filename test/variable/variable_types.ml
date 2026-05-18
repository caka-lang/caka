open Alcotest
open Caka_parser
open Caka_parser.Utils

let cases =
  [
    test_case "bool" `Quick (fun () ->
        let code = "x : bool = false" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TBool ());
                 value = Mocks.mk_expr (`Bool false);
               }));
    test_case "int" `Quick (fun () ->
        let code = "x : int = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TInt 32);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "i8" `Quick (fun () ->
        let code = "x : i8 = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TInt 8);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "i16" `Quick (fun () ->
        let code = "x : i16 = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TInt 16);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "i32" `Quick (fun () ->
        let code = "x : i32 = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TInt 32);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "i64" `Quick (fun () ->
        let code = "x : i64 = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TInt 64);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "i128" `Quick (fun () ->
        let code = "x : i128 = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TInt 128);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "uint" `Quick (fun () ->
        let code = "x : uint = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TUint 32);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "u8" `Quick (fun () ->
        let code = "x : u8 = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TUint 8);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "u16" `Quick (fun () ->
        let code = "x : u16 = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TUint 16);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "u32" `Quick (fun () ->
        let code = "x : u32 = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TUint 32);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "u64" `Quick (fun () ->
        let code = "x : u64 = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TUint 64);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "u128" `Quick (fun () ->
        let code = "x : u128 = 0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TUint 128);
                 value = Mocks.mk_expr (`Int 0);
               }));
    test_case "float" `Quick (fun () ->
        let code = "x : float = 0.0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TFloat 64);
                 value = Mocks.mk_expr (`Float 0.0);
               }));
    test_case "f32" `Quick (fun () ->
        let code = "x : f32 = 0.0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TFloat 32);
                 value = Mocks.mk_expr (`Float 0.0);
               }));
    test_case "f64" `Quick (fun () ->
        let code = "x : f64 = 0.0" in
        let m = code |> Lexer.resolve |> List.hd in
        check Mocks.stmt_testable "name is x with type int and value none"
          (List.hd m.block.statements)
          (Mocks.mk_stmt
          @@ `Let
               {
                 name = "x";
                 mut = false;
                 ty = Mocks.mk_types (`TFloat 64);
                 value = Mocks.mk_expr (`Float 0.0);
               }));
  ]
