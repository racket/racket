#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/contract
                     "private/ppict-syntax.rkt")
         racket/contract
         slideshow/pict
         "private/ppict.rkt")

(define-syntax (ppict-do stx)
  (syntax-parse stx
    [(_ base p ...)
     #:declare base (expr/c #'pict?)
     #:declare p (fragment 'ppict-do)
     #'(let-values ([(final _picts)
                     (internal-ppict-do 'ppict-do base.c (list p.code ...))])
         final)]))

(define-syntax (ppict-do* stx)
  (syntax-parse stx
    [(_ base p ...)
     #:declare base (expr/c #'pict?)
     #:declare p (fragment 'ppict-do)
     #'(internal-ppict-do 'ppict-do* base.c (list p.code ...))]))

;; ----

(provide ppict-do
         ppict-do*)

(provide ppict?
         placer?)

(provide/contract
 [ppict-go
  (-> pict? placer? ppict?)]
 [ppict-add
  (->* (ppict?)
       ()
       #:rest (listof (or/c pict? real? #f))
       pict?)]
 [ppict-placer
  (-> ppict? placer?)]
 [placer
  (-> any/c boolean?)]
 [coord
  (->* (real? real?)
       (align/c
        #:compose procedure?)
       placer?)]
 [grid
  (->* (exact-positive-integer? exact-positive-integer?
        exact-nonnegative-integer? exact-nonnegative-integer?)
       (align/c
        #:compose procedure?)
       placer?)])
