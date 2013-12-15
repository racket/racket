#lang racket/base

;; Reprovides everything from all the files in the combinators directory.

(require (for-syntax racket/base racket/runtime-path))

(begin-for-syntax
  (define-runtime-path combinator-dir "combinators")
  (define base-file-names
    (filter (lambda (v) (regexp-match? #rx".rkt$" v)) (directory-list combinator-dir)))
  (define file-names (map (lambda (v) (string-append "combinators/" (path->string v)))
                          base-file-names)))

(define-syntax (gen-provides stx)
  #`(begin
      (require #,@file-names)
      (provide (all-from-out #,@file-names))))

(gen-provides)
