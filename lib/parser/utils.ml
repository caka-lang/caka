open Ast

let mk_position (position : Lexing.position) =
  Position.
    {
      fname = position.pos_fname;
      lnum = position.pos_lnum;
      bol = position.pos_bol;
      cnum = position.pos_cnum;
    }

let mk_types position node : Type.t = { position = mk_position position; node }

let mk_expr position node : Expression.t =
  Expression.{ position = mk_position position; node }

let mk_stmt position node : Statement.t =
  Statement.{ position = mk_position position; node }

let pp_ast_position (p : Position.t) =
  Printf.sprintf "%s:%d:%d" p.fname p.lnum (p.cnum - p.bol + 1)

let t_void ~position = Type.make ~position ~node:(`TVoid ()) ()

module EnvManager = struct
  type t = (string, Statement.t) Hashtbl.t list

  let create () : t = [ Hashtbl.create 10 ]
  let push (env : t) : t = Hashtbl.create 10 :: env
  let pop (env : t) : t = match env with _ :: rest -> rest | [] -> []

  let add (env : t) name symbol : t =
    match env with
    | curr :: rest ->
        Hashtbl.replace curr name symbol;
        env
    | [] -> []

  let rec find (env : t) (name : string) =
    match env with
    | [] -> None
    | curr :: outer -> (
        match Hashtbl.find_opt curr name with
        | None -> find outer name
        | Some symbol -> Some symbol)
end
