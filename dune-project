(lang dune 3.16)

(using menhir 3.0)

(name ocr_computing_challenges)

(generate_opam_files true)

(source
 (github AlanRandon/ocr-computing-challenges))

(authors "Alan Randon")

(maintainers "Alan Randon")

(package
 (name ocr_computing_challenges)
 (synopsis "OCR computing challenges")
 (description "OCR computing challenges from GCSE to A-level")
 (depends
   ocaml
   dune
   core
   core_unix
   ppx_jane
   ppx_show
   ppx_deriving
   ppx_inline_test
   yojson
   ppx_deriving_yojson
   otoml
   nottui
   nottui-pretty
   calendar))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
