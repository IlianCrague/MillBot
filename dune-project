(lang dune 3.10)

(name mill)

(generate_opam_files true)

(source
 (uri git+https://gaufre.informatique.univ-paris-diderot.fr/paris/ppf5-jeudumoulin-ocaml))

(authors "ESPANTET Nicolas, LEFORESTIER Meril, PARIS Albin, YAZICI Servan")

(maintainers "ESPANTET Nicolas, LEFORESTIER Meril, PARIS Albin, YAZICI Servan")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name mill)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune alcotest qcheck qcheck-alcotest))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
