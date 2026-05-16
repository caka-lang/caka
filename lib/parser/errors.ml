exception Error of string

let unknown_function name =
  Error (Printf.sprintf "unable to find function `%s`" name)
