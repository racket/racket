;; copyright by Paul Graunke June 2000 AD
#lang scheme

(define-signature sgml-reader^ (read-html-comments trim-whitespace gen-may-contain gen-read-sgml))

(provide sgml-reader^)
