#lang s-exp syntax/module-reader

scribble/doclang

;; `read-inside' reads the whole body, so make wrapper1 return null so
;; we get the right syntax, and then make wrapper2 do the actual
;; reading.  This might seem extreme, but I think that it's still
;; better to use module-reader for the subtleties it deals with.

#:wrapper1 (lambda (t) '())

#:wrapper2
(lambda (in read stx?)
  (let* ([skeleton (read in)]
         [skeleton (if stx? (syntax->list skeleton) skeleton)]
         [body (if stx?
                 (scribble:read-syntax-inside (object-name in) in)
                 (scribble:read-inside in))]
         [mod  `(,(car skeleton) ,(cadr skeleton) ,(caddr skeleton)
                 (#%module-begin doc () . ,body))])
    (if stx? (datum->syntax #f mod) mod)))

(require (prefix-in scribble: "../reader.ss"))
