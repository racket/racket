#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/contract
                     "private/ppict-syntax.rkt")
         racket/list
         racket/contract
         racket/gui/base
         slideshow/base
         slideshow/pict
         "private/ppict.rkt")

;; ============================================================
;; Progressive Slides

(define pslide-base-pict
  (make-parameter (lambda () (blank client-w client-h))))

(define pslide-default-placer
  (make-parameter (coord 1/2 1/2 'cc)))

;; pslide* : (U p:elem p:out p:go) ... -> void
(define (pslide* who parts)
  (let* ([init-go (p:go (pslide-default-placer))]
         [init-pict ((pslide-base-pict))]
         [gochunks
          (get-gochunks who init-go (append parts (list (p:out))))])
    (let-values ([(final picts) (do-gochunks init-pict gochunks)])
      (for-each slide picts)
      (void))))

;; ----

(define-syntax (pslide stx)
  (syntax-parse stx
    [(_ p ...)
     #:declare p (fragment 'pslide)
     #'(pslide* 'pslide (list p.code ...))]))

;; ============================================================
;; Exports

(provide/contract
 [pslide-base-pict
  (parameter/c (-> pict?))]
 [pslide-default-placer
  (parameter/c placer?)])

(provide pslide)
