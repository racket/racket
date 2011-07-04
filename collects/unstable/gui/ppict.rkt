#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/parse/experimental/contract
                     "private/ppict-syntax.rkt")
         racket/contract
         slideshow/pict
         "private/ppict.rkt")

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
         refpoint-placer?
         tag-path?)

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
      refpoint-placer?)]

 [tag-pict
  (-> pict? symbol? pict?)]
 [pict-tag
  (-> pict? (or/c symbol? #f))]
 [find-tag
  (-> pict? tag-path? (or/c pict-path? #f))])
