#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/contract
                     "private/ppict-syntax.rkt")
         racket/contract/base
         pict
         unstable/gui/pict/align
         "private/ppict.rkt"
         "private/tag-pict.rkt")

(define-for-syntax (ppict-do*-transformer who stx)
  (syntax-parse stx
    [(_ base . fs)
     #:declare base (expr/c #'pict?)
     #:declare fs (fragment-sequence who #'xp #'rpss)
     #'(let ([xp base.c] [rpss null])
         fs.code)]))

(define-syntax (ppict-do stx)
  #`(let-values ([(final _picts)
                  #,(ppict-do*-transformer 'ppict-do stx)])
      final))

(define-syntax (ppict-do* stx)
  (ppict-do*-transformer 'ppict-do* stx))

;; ----

(provide ppict-do
         ppict-do*
         ppict-do-state)

(provide ppict?
         placer?
         refpoint-placer?)

(provide/contract
 [ppict-go
  (-> pict? placer? ppict?)]
 [ppict-add
  (->* (ppict?)
       ()
       #:rest (listof (or/c pict? real? #f 'next))
       pict?)]
 [ppict-add*
  (->* (ppict?)
       ()
       #:rest (listof (or/c pict? real? #f 'next))
       (values pict? (listof pict?)))]
 [ppict-placer
  (-> ppict? placer?)]
 [coord
  (->* (real? real?)
       (align/c
        #:abs-x real?
        #:abs-y real?
        #:compose procedure?)
       refpoint-placer?)]
 [grid
  (->* (exact-positive-integer? exact-positive-integer?
        exact-integer? exact-integer?)
       (align/c
        #:abs-x real?
        #:abs-y real?
        #:compose procedure?)
       refpoint-placer?)]
 [cascade
  (->* ()
       ((or/c real? 'auto) (or/c real? 'auto))
       placer?)]
 [tile
  (-> exact-positive-integer? exact-positive-integer?
      placer?)]
 [at-find-pict
  (->* ((or/c tag-path? pict-path?))
       (procedure?
        align/c
        #:abs-x real?
        #:abs-y real?
        #:compose procedure?)
       refpoint-placer?)]
 [merge-refpoints
  (-> refpoint-placer? refpoint-placer?
      refpoint-placer?)])
