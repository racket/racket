#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "private/ppict-syntax.rkt")
         racket/contract/base
         slideshow/base
         pict
         "private/ppict.rkt")

;; ============================================================
;; Progressive Slides

(define pslide-base-pict
  (make-parameter (lambda () (blank client-w client-h))))

(define pslide-default-placer
  (make-parameter (coord 1/2 1/2 'cc)))

;; pslide* : symbol (pict -> (values pict (listof pict)) -> void
(define (pslide* who proc)
  (let* ([init-pict ((pslide-base-pict))]
         [init-placer (pslide-default-placer)])
    (let-values ([(final picts)
                  (proc (ppict-go init-pict init-placer))])
      (for-each slide picts)
      (slide final)
      (void))))

;; ----

(define-syntax (pslide stx)
  (syntax-parse stx
    [(_ . fs)
     #:declare fs (fragment-sequence 'pslide #'xp #'rpss)
     #'(pslide* 'pslide
                (lambda (xp)
                  (let ([rpss null])
                    fs.code)))]))

;; ============================================================
;; Exports

(provide/contract
 [pslide-base-pict
  (parameter/c (-> pict?))]
 [pslide-default-placer
  (parameter/c placer?)])

(provide pslide)
