open Caka_parser.Ast
open Caka_parser.Utils

let mk_position = mk_position Lexing.dummy_pos
let mk_types node = mk_types Lexing.dummy_pos node
let mk_expr node = mk_expr Lexing.dummy_pos node
let mk_stmt node = mk_stmt Lexing.dummy_pos node
let t_void = Type.make ~position:mk_position ~node:(`TVoid ()) ()

module Clean = struct
  let rec typ (ty : Type.t) : Type.t =
    let node =
      match ty.node with
      | `TArray a -> `TArray { a with ty = typ a.ty }
      | `TFn f -> `TFn { f with ty = typ f.ty; params = List.map typ f.params }
      | `TStruct s ->
          `TStruct
            {
              s with
              fields =
                List.map
                  (fun (f : StructField.t) -> { f with ty = typ f.ty })
                  s.fields;
            }
      | o -> o
    in
    { node; position = mk_position }

  and expr (e : Expression.t) : Expression.t =
    let node =
      match e.node with
      | `Var v -> `Var { v with value = Option.map expr v.value }
      | `Add b -> `Add (binop b)
      | `Sub b -> `Sub (binop b)
      | `Mul b -> `Mul (binop b)
      | `Div b -> `Div (binop b)
      | `Mod b -> `Mod (binop b)
      | `Pow b -> `Pow (binop b)
      | `Eq b -> `Eq (binop b)
      | `Ne b -> `Ne (binop b)
      | `Lt b -> `Lt (binop b)
      | `Lte b -> `Lte (binop b)
      | `Gt b -> `Gt (binop b)
      | `Gte b -> `Gte (binop b)
      | `Array a ->
          let aa : EArray.t =
            { items = List.map expr a.items; ty = Option.map typ a.ty }
          in
          `Array aa
      | `Fn f ->
          let ff : EFn.t =
            {
              ty = (match f.ty with Some fty -> Some (typ fty) | _ -> None);
              params =
                List.map
                  (fun (p : Param.t) -> { p with ty = typ p.ty })
                  f.params;
              block =
                { f.block with statements = List.map stmt f.block.statements };
            }
          in
          `Fn ff
      | o -> o
    in
    let e : Expression.t = { node; position = mk_position } in
    e

  and binop (e : EBinop.t) : EBinop.t = { l = expr e.l; r = expr e.r }
  and param (e : Param.t) : Param.t = { e with ty = typ e.ty }

  and stmt (s : Statement.t) : Statement.t =
    let node =
      match s.node with
      | `Let l -> `Let { l with ty = typ l.ty; value = expr l.value }
      | `Return r -> `Return (expr r)
      | `Block b ->
          let bb : SBlock.t =
            { statements = List.map stmt b.statements; ty = b.ty }
          in
          `Block bb
      | `Type t -> `Type { t with ty = typ t.ty }
      | o -> o
    in
    let s : Statement.t = { node; position = mk_position } in
    s
end

let equal_statement a b = Clean.stmt a = Clean.stmt b
let stmt_testable = Alcotest.testable Statement.pp equal_statement
