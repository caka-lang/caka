open Alcotest

let () =
  run "caka"
    [
      ("variable", Variable.cases);
      ("variable types", Variable_types.cases);
      ("variable types cast", Variable_types_cast.cases);
      ("variable types compound", Variable_types_compound.cases);
      ("operator math", Operator_math.cases);
      ("operator comparation", Operator_comparation.cases);
      ("function", Function.cases);
      ("function types", Function_types.cases);
      ("call", Call.cases);
      ("types named", Types_named.cases);
    ]
