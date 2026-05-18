open Ast

let rec of_expr (e : Expression.t) : Type.t =
  let position = e.position in
  match e.node with
  | `Int i -> Type.make ~position ~node:(`TInt 32) ()
  | `Float i -> Type.make ~position ~node:(`TFloat 64) ()
  | `Bool _ -> Type.make ~position ~node:(`TBool ()) ()
  | `Eq _ | `Ne _ | `Lt _ | `Lte _ | `Gt _ | `Gte _ ->
      Type.make ~position ~node:(`TBool ()) ()
  | `Add { l; _ }
  | `Sub { l; _ }
  | `Mul { l; _ }
  | `Mod { l; _ }
  | `Pow { l; _ } ->
      of_expr l
  | `Div { l; _ } -> Type.make ~position ~node:(`TFloat 64) ()
  | `Fn f ->
      Type.make ~position
        ~node:
          (`TFn
             {
               ty =
                 (match f.ty with
                 | Some ft -> Type.make ~position ~node:ft.node ()
                 | _ -> Utils.t_void ~position);
               params = List.map (fun (fp : Param.t) -> fp.ty) f.params;
               name = None;
             })
        ()
  | _ -> Utils.t_void ~position

and default_value (t : Type.t) : Expression.t =
  let position = t.position in
  match t.node with
  | `TInt i -> Expression.make ~position ~node:(`Int 0) ()
  | _ -> Expression.make ~position ~node:(`Nil ()) ()
