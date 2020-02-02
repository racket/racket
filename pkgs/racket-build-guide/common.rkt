#lang racket/base
(require scribble/base
         scribble/bnf
         scribble/core)

(provide (all-defined-out)
         nonterm)

(define git-repo "https://github.com/racket/racket")

;; Unlike `exec` from `scribble/manual`, apply
;; 'tt to all arguments, because that looks right
;; for markdown output:
(define (exec . s)
  (element 'tt s))

;; Ditto
(define (commandline . s)
  (para (hspace 2) (element 'tt s)))
