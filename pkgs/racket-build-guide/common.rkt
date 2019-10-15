#lang racket/base
(require scribble/base
         scribble/bnf)

(provide (all-defined-out)
         nonterm)

(define git-repo "https://github.com/racket/racket")

;; Unlike `exec` from `scribble/manual`, apply
;; 'tt to all arguments, because that looks right
;; for markdown output:
(define (exec . s)
  (elem #:style 'tt s)) 

;; Ditto
(define (commandline . s)
  (para (hspace 2) (elem #:style 'tt s)))
