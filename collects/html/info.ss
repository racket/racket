#lang setup/infotab

(define name "HTML")
(define scribblings '(("html.scrbl")))
(define compile-omit-files
  '("dtd.ss" "dtdr.ss" "dtds.ss" "dtd-ast.ss" "case.ss" "html-structs.ss"
    "entity-expander.ss" "generate-code.ss" "sgml.ss"))
