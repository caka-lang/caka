open Cmdliner

let cmd =
  let doc = "Caka-lang - A Good Programming Language" in
  let info = Cmd.info "caka" ~version:"0.1.0" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [ Debug.command; Run.command ]

let () = exit (Cmd.eval cmd)
