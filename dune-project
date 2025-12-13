(lang dune 3.17)

(name muaddebt)

(generate_opam_files true)

(source
 (github cz4rny/muaddebt))

(authors "Cezary Krzyżanowski <muaddebt@thezenofit.com")

(maintainers "Cezary Krzyżanowski <muaddebt@thezenofit.com")

; (license LICENSE)
;
; (documentation https://url/to/documentation)

(package
 (name muaddebt)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml
   (>= 5.4.0))
  dune
  (ppx_deriving
   (>= 6.1.1))
  (ppx_enumerate
   (>= 0.17.0))
  ;; Dev deps
  ocaml-lsp-server
  merlin
  ocamlformat
  odig)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
