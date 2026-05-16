open Cmdliner
open Caka_parser

external codegen : string -> int -> string = "codegen"

let read file = In_channel.with_open_text file In_channel.input_all

type debug_type = AST | IR

let execute ty =
  let f = read "./main.ck" in
  let code = f in
  let prog = Resolver.resolve code in
  match ty with
  | Some AST -> print_endline @@ Ast.Program.show prog
  | _ ->
      let generated =
        List.map
          (fun m ->
            m |> Ast.Module.to_proto |> fun mproto ->
            let b = Ocaml_protoc_plugin.Writer.contents mproto in
            codegen b (String.length b))
          prog
      in
      List.iter (fun s -> print_endline s) generated

let flag_type =
  let values = Arg.enum [ ("ast", AST); ("ir", IR) ] in
  Arg.(value & opt (some values) None & info [ "type" ] ~docv:"TYPE")

let command =
  let doc = "Read caka-lang code file then show debug output" in
  let info = Cmd.info "debug" ~doc in
  Cmd.v info Term.(const execute $ flag_type)
