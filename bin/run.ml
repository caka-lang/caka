open Cmdliner
open Caka_parser

external codegen : string -> int -> string = "codegen"
external compile : string -> int -> string = "compile"

let read file = In_channel.with_open_text file In_channel.input_all

let write file content =
  let oc = open_out file in
  Printf.fprintf oc "%s\n" content;
  close_out oc

let execute () =
  let f = read "./main.ck" in
  let code = f in
  let prog = Resolver.resolve code in
  let generated =
    List.map
      (fun m ->
        m |> Ast.Module.to_proto |> fun mproto ->
        let b = Ocaml_protoc_plugin.Writer.contents mproto in
        codegen b (String.length b))
      prog
  in
  let compiled_files =
    List.fold_left
      (fun acc s -> compile s (String.length s) :: acc)
      [] generated
  in
  ignore @@ Sys.command
  @@ Printf.sprintf "clang %s ./std/builtin.c -o main"
       (String.concat " " compiled_files);
  ignore @@ exit @@ Sys.command "./main"

let command =
  let doc = "Read caka-lang code file then show debug output" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(const execute $ const ())
