(lang dune 3.22)

(name caka)

(generate_opam_files true)

(source
 (github caka-lang/caka))

(authors "hadihammurabi <hadihammurabi+caka@gmail.com>")

(maintainers "hadihammurabi <hadihammurabi+caka@gmail.com>")

(license "GPL-3.0-or-later")

(documentation https://github.com/caka-lang)

(package
 (name caka)
 (license "GPL-3.0-or-later")
 (synopsis "A good programming language")
 (description "A good programming language")
 (tags ())
 (depends 
   ocaml 
   ocamlformat
   cmdliner 
   fmt 
   linol 
   linol-lwt 
   lsp
   ( alcotest (>= "1.9.1" ) )
   sedlex
   ocaml-protoc-plugin
  )
)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
(using menhir 3.0)
